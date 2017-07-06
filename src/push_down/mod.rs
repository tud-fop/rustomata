extern crate num_traits;

use std::collections::{BinaryHeap, HashMap};
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use num_traits::{One, Zero};
use std::ops::{Add, Mul};

use automata;

pub mod from_cfg;
pub mod relabel;
pub mod red;

pub use from_cfg::*;
pub use approximation::relabel::*;
pub use push_down::red::*;


/// Automaton with storage type `PushDown<A>`, terminals of type `T` and weights of type `W`.
#[derive(Debug, Clone)]
pub struct PushDownAutomaton<A: Ord + PartialEq + Debug + Clone + Hash, T: Eq, W: Ord + Eq> {
    pub transitions: HashMap<A, BinaryHeap<automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>>,
    pub initial: PushDown<A>,
}

/// Instruction on `PushDown<A>`s.
#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum PushDownInstruction<A> {
    Replace { current_val: Vec<A>, new_val : Vec<A>},
    ReplaceK { current_val: Vec<A>, new_val : Vec<A>, limit : usize},
}

/// Stack with Elements of type `A`
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PushDown<A: Ord> {
    pub elements: Vec<A>,
    pub empty: A,
}

impl<A: Ord + PartialEq + Debug + Clone + Hash,
    T: Eq + Clone + Hash,
    W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Zero +One> PushDownAutomaton<A, T, W> {
    pub fn new(transitions: Vec<automata::Transition<PushDown<A>,PushDownInstruction<A>, T, W>>, initial: PushDown<A>)
            -> PushDownAutomaton<A,T,W>{

        let mut transition_map: HashMap< A, BinaryHeap<automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>>  = HashMap::new();

        for t in transitions {
            let a =
                match t.instruction {
                    PushDownInstruction::Replace { ref current_val, ..} => current_val.first().unwrap().clone(),
                    PushDownInstruction::ReplaceK { ref current_val, ..} => current_val.first().unwrap().clone(),
                };

            if !transition_map.contains_key(&a) {
                transition_map.insert(a.clone(), BinaryHeap::new());
                ()
            }

            transition_map.get_mut(&a).unwrap().push(t.clone());

            let b = initial.empty.clone();

            if !transition_map.contains_key(&b) {
                transition_map.insert(b.clone(), BinaryHeap::new());
                ()
            }
            transition_map.get_mut(&b).unwrap().push(t);
        }

        let p = PushDownAutomaton {
            transitions: transition_map,
            initial: initial,
        };
        p.reduce_redundancy()
    }

    pub fn list_transitions(&self) -> Vec<&automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>> {
        let mut result = Vec::new();
        let mut keys: Vec<_> = self.transitions.keys().collect();

        keys.sort();

        for k in keys {
            if *k != self.initial.empty{
                for t in self.transitions.get(k).unwrap() {
                    result.push(t);
                }
            }
        }

        result
    }
}

impl<A: Ord + PartialEq + Debug + Clone + Hash> automata::Instruction<PushDown<A>>
    for PushDownInstruction<A> {
        fn apply(&self, p: PushDown<A>) -> Vec<PushDown<A>> {
            match self {
                &PushDownInstruction::Replace {ref current_val, ref new_val} => {
                    p.replace(current_val, new_val)
                }
                &PushDownInstruction::ReplaceK {ref current_val, ref new_val, ref limit} => {
                    p.replacek(current_val, new_val, limit)
                }
            }
        }

}

impl<A: Ord + PartialEq + Debug + Clone + Hash,
     T: Clone + Debug + Eq + Hash,
     W: One + Mul<Output=W> + Clone + Copy + Debug + Eq + Ord>
    automata::Automaton<PushDown<A>, PushDownInstruction<A>, T, W> for PushDownAutomaton<A, T, W> {
        type Key = A;

        fn extract_key(c: &automata::Configuration<PushDown<A>, T, W>) -> &A {
            if c.storage.is_bottom(){
                return &c.storage.empty;
            }else{
                return c.storage.current_symbol();
            }
        }

        fn transitions(&self) -> &HashMap<A, BinaryHeap<automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>> {
            &self.transitions
        }

        fn initial(&self) -> PushDown<A> {
            self.initial.clone()
        }

        fn is_terminal(&self, c: &automata::Configuration<PushDown<A>, T, W>) -> bool{
            c.word.is_empty() && c.storage.is_bottom()
        }
}

impl<A: Ord + PartialEq + Clone + Debug> PushDown<A> {
    ///new `PushDown<A>` stack with empty-symbol of type `A` and initial symbol of type `A`
    pub fn new(a: A, empty: A)->PushDown<A>{
        let mut ele : Vec<A> = Vec::new();
        ele.push(empty.clone());
        ele.push(a.clone());
        PushDown{
            elements : ele,
            empty : empty,
        }
    }

    pub fn current_symbol(&self) -> &A {
        let n= self.elements.len();
        &self.elements[n-1]
    }

    /// checks wheter stack is empty, meaning bottomsymbol is at top
    pub fn is_bottom(&self) -> bool{
        *self.current_symbol() == self.empty
    }

    /// Opertations for Instructions:

    ///replaces uppermost element with the given elements.
    pub fn replace(&self, cur_sym: &Vec<A>,  new_sym: &Vec<A>) -> Vec<PushDown<A>>{
        let mut new_elements=self.elements.clone();

        for c in cur_sym{
            if !(new_elements.last()==Some(c)){
                return Vec::new();
            }
            new_elements.pop();
        }

        for e in new_sym{
            new_elements.push(e.clone());
        }
        vec![PushDown{
            elements: new_elements,
            empty: self.empty.clone(),
        }]
    }

    ///replaces uppermost element with the given elements. Truncates to the last `limit` elements. Also works without `cur_sym`if `is_bottom()`
    pub fn replacek(&self, cur_sym: &Vec<A>,  new_sym: &Vec<A>, limit: &usize) -> Vec<PushDown<A>>{
        let mut new_elements=self.elements.clone();

        if !(self.is_bottom()){
            for c in cur_sym{
                if !(new_elements.last()==Some(c)){
                    return Vec::new();
                }
                new_elements.pop();
            }
        }
        for e in new_sym{
            new_elements.push(e.clone());
        }
        let n = new_elements.len();

        if n>*limit{
            let last_elements = new_elements.split_off(n-*limit);
            new_elements = last_elements;
            new_elements.insert(0,self.empty.clone());
        }

        vec![PushDown{
            elements: new_elements,
            empty: self.empty.clone(),
        }]
    }
}


impl<A: fmt::Display> fmt::Display for PushDownInstruction<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &PushDownInstruction::Replace { ref current_val, ref new_val} => {
                let mut buffer1 = "".to_string();
                let mut buffer2 = "".to_string();

                let mut iter1 = current_val.iter().peekable();
                let mut iter2 = new_val.iter().peekable();

                while let Some(nt) = iter1.next() {
                    buffer1.push_str(format!("\"{}\"", nt).as_str());
                    if iter1.peek().is_some() {
                        buffer1.push_str(", ");
                    }
                }

                while let Some(nt) = iter2.next() {
                    buffer2.push_str(format!("\"{}\"", nt).as_str());
                    if iter2.peek().is_some() {
                        buffer2.push_str(", ");
                    }
                }
                write!(f, "(Replace {} // {})", buffer1, buffer2)
            }
            &PushDownInstruction::ReplaceK { ref current_val, ref new_val , ref limit} => {
                let mut buffer1 = "".to_string();
                let mut buffer2 = "".to_string();

                let mut iter1 = current_val.iter().peekable();
                let mut iter2 = new_val.iter().peekable();

                while let Some(nt) = iter1.next() {
                    buffer1.push_str(format!("\"{}\"", nt).as_str());
                    if iter1.peek().is_some() {
                        buffer1.push_str(", ");
                    }
                }

                while let Some(nt) = iter2.next() {
                    buffer2.push_str(format!("\"{}\"", nt).as_str());
                    if iter2.peek().is_some() {
                        buffer2.push_str(", ");
                    }
                }
                write!(f, "(ReplaceK {} // {} {})", buffer1, buffer2, limit)
            }
        }
    }
}


impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash + fmt::Display,
     T: Clone + fmt::Debug + Eq + Hash,
     W: One + Clone + Copy + fmt::Debug + Eq + Ord + fmt::Display + Add<Output=W> + Mul<Output = W> + Zero>
    fmt::Display for PushDownAutomaton<A, T, W> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let mut formatted_transitions = String::new();
            for t in self.list_transitions() {
                formatted_transitions.push_str(&t.to_string());
                formatted_transitions.push_str("\n");
            }
            write!(f, "initial: {}\n\n{}", self.initial.current_symbol(), formatted_transitions)
        }
    }
