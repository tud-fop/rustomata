extern crate num_traits;

use std::collections::{BinaryHeap, HashMap};
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use num_traits::{One, Zero};
use std::ops::{Add, Mul, Div};

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
    Pop { current_val: A , new_val : Option<A>},
    Replace { current_val: A, new_val : Vec<A>},
}

/// Stack with Elements of type `A`
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PushDown<A: Ord> {
    pub elements: Vec<A>,
    pub limit: i64,
    pub empty: A,
}

impl<A: Ord + PartialEq + Debug + Clone + Hash,
    T: Eq + Clone + Hash,
    W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Add<f64, Output = f64> + Zero +One> PushDownAutomaton<A, T, W> {
    pub fn new(transitions: Vec<automata::Transition<PushDown<A>,PushDownInstruction<A>, T, W>>,initial: PushDown<A>)
            -> PushDownAutomaton<A,T,W>{

        let mut transition_map: HashMap<A, BinaryHeap<automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>>  = HashMap::new();

        for t in transitions {
            let a =
                match t.instruction {
                    PushDownInstruction::Pop { ref current_val, ..} => current_val.clone(),
                    PushDownInstruction::Replace { ref current_val, ..} => current_val.clone(),
                };

            if !transition_map.contains_key(&a) {
                transition_map.insert(a.clone(), BinaryHeap::new());
                ()
            }

            transition_map.get_mut(&a).unwrap().push(t);
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
            for t in self.transitions.get(k).unwrap() {
                result.push(t);
            }
        }

        result
    }
}

impl<A: Ord + PartialEq + Debug + Clone + Hash> automata::Instruction<PushDown<A>>
    for PushDownInstruction<A> {
        fn apply(&self, p: PushDown<A>) -> Vec<PushDown<A>> {
            match self {
                &PushDownInstruction::Pop {ref current_val, ref new_val} => {
                    p.pop(current_val, new_val)
                }
                &PushDownInstruction::Replace {ref current_val, ref new_val} => {
                    p.replace(current_val, new_val)
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
            c.storage.current_symbol()
        }

        fn transitions(&self) -> &HashMap<A, BinaryHeap<automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>> {
            &self.transitions
        }

        fn initial(&self) -> PushDown<A> {
            self.initial.clone()
        }

        fn is_terminal(&self, c: &automata::Configuration<PushDown<A>, T, W>) -> bool{
            c.word.is_empty() && (c.storage.elements.len() == 1)
        }
}

impl<A: Ord + PartialEq + Clone + Debug> PushDown<A> {
    ///new `PushDown<A>` stack with empty-symbol of type `A` and initial symbol of type `A`
    pub fn new(a: A, b : A, l: i64)->PushDown<A>{
        let mut ele : Vec<A> = Vec::new();
        ele.push(a.clone());
        ele.push(b.clone());
        let li;
        if l < 0{
            li = -1;
        }else{
            li = l;
        }
        PushDown{
            elements : ele,
            limit: li,
            empty: a,
        }
    }

    pub fn current_symbol(&self) -> &A {
        let n= self.elements.len();
        &self.elements[n-1]
    }

    //checks wheter stack has reached limit
    pub fn is_full(&self) ->bool{
        if self.limit < 0{
            return false;
        }
        (self.elements.len() as i64) >= (self.limit+1)
    }

    /// checks wheter stack is empty, meaning bottomsymbol is at top
    pub fn is_bottom(&self) -> bool{
        *self.current_symbol()==self.empty
    }

    /// Opertations for Instructions:

    ///pushes new element at the top
    pub fn push(&self,o: &A, n: &A) -> Vec<PushDown<A>>{
        if !(o==self.current_symbol()){
            return Vec::new()
        }
        let mut s=self.elements.clone();
        s.push(n.clone());

        vec![PushDown{
            elements: s,
            empty: self.empty.clone(),
        }]
    }

    ///pops uppermost element, returns `None` if empty
    pub fn pop(&self, c: &A) -> Vec<PushDown<A>>{
        if self.is_bottom(){
            return Vec::new()
        }

        if !(self.current_symbol()==c){
            println!("nooo");
            return Vec::new()
        }

        let mut b=self.elements.clone();
        b.pop();
        vec![PushDown{
            elements: b,
            limit: self.limit,
            empty: self.empty.clone(),

        }]

    }

    ///replaces uppermost element with the given elements, returns `None` if empty. Inverts the given Vector. Does Nothing when empty input.
    pub fn replace(&self, c: &A,  a: &Vec<A>) -> Vec<PushDown<A>>{
        if a.len() == 0{
            return vec![self.clone()]
        }

        if self.is_bottom(){
            return Vec::new()
        }

        if !(self.current_symbol()==c){
            return Vec::new()
        }

        let mut b=self.elements.clone();
        b.pop();
        let mut inva = Vec::new();
        let mut copa = a.clone();
        loop{
            if copa.len()==0{
                break;
            }
            inva.push(copa.pop().unwrap());
        }

        for x in inva{
            b.push(x.clone());
        }
        vec![PushDown{
            elements: b,
            limit: self.limit.clone(),
            empty: self.empty.clone(),
        }]
    }
}

impl<A: fmt::Display> fmt::Display for PushDownInstruction<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &PushDownInstruction::Pop { ref current_val, ref new_val} => {
                match new_val{
                    &Some( ref x) => write!(f, "(Pop {}, uncover {})", current_val, x),
                    &None => write!(f, "(Pop {})", current_val)
                }
            },
            &PushDownInstruction::Replace { ref current_val, ref new_val } => {
                let mut buffer = "".to_string();

                let mut iter = new_val.iter().peekable();

                while let Some(nt) = iter.next() {
                    buffer.push_str(format!("\"{}\"", nt).as_str());
                    if iter.peek().is_some() {
                        buffer.push_str(", ");
                    }
                }
                write!(f, "(Replace {} {})", current_val, buffer)
            }
        }
    }
}


impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash + fmt::Display,
     T: Clone + fmt::Debug + Eq + Hash,
     W: One + Clone + Copy + fmt::Debug + Eq + Ord + fmt::Display + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Add<f64, Output = f64> + Zero>
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
