extern crate num_traits;

use std::collections::{BinaryHeap, HashMap};
use std::fmt;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::slice::Iter;
use std::vec::Vec;
use num_traits::{One, Zero};
use std::ops::{Add, Mul, Div};

use automata::{Automaton, Configuration, Instruction, Transition};
use automata::red::*;

pub mod from_cfg;
pub mod relabel;
pub mod red;

pub use self::from_cfg::*;
pub use self::relabel::*;
pub use self::red::*;

type TransitionMap<A, T, W>
    = HashMap<A, BinaryHeap<Transition<PushDownInstruction<A>, T, W>>>;

/// Automaton with storage type `PushDown<A>`, terminals of type `T` and weights of type `W`.
#[derive(Debug, Clone)]
pub struct PushDownAutomaton<A, T, W>
    where A: Clone + Debug + Hash + Ord,
          T: Eq,
          W: Ord,
{
    pub transitions: TransitionMap<A, T, W>,
    pub initial: PushDown<A>,
}

/// Instruction on `PushDown<A>`s.
#[derive(PartialEq, Eq, Clone, Debug, Hash, PartialOrd, Ord)]
pub enum PushDownInstruction<A> {
    Replace { current_val: Vec<A>, new_val: Vec<A> },
    ReplaceK { current_val: Vec<A>, new_val: Vec<A>, limit: usize },
}

/// Stack with Elements of type `A`
#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord)]
pub struct PushDown<A> {
    elements: Vec<A>,
    empty: A,
}

impl<A, T, W> PushDownAutomaton<A, T, W>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          T: Eq + Clone + Hash,
          W: Ord + Eq + Clone + Add<Output=W> + Mul<Output=W> + Div<Output=W> + Zero + One,
{
    pub fn new(transitions: Vec<Transition<PushDownInstruction<A>, T, W>>,
               initial: PushDown<A>)
               -> PushDownAutomaton<A,T,W>
    {
        let mut transition_map = HashMap::new();
        let emp_transitions = transitions.len();
        let mut nw = W::one();
        for _ in 2..emp_transitions{
            nw = nw+W::one();
        }
        let b = initial.empty.clone();

        for t in transitions {
            let mut emp = false;
            let a =
                match t.instruction {
                    PushDownInstruction::Replace { ref current_val, .. } => current_val.first().unwrap().clone(),
                    PushDownInstruction::ReplaceK { ref current_val, .. } => {
                        emp = true;
                        current_val.first().unwrap().clone()
                    },
                };

            transition_map.entry(a).or_insert_with(BinaryHeap::new).push(t.clone());

            //Places all ReplaceK transitions also in for the empty symbol
            if emp {
                let nt = Transition {
                    word: t.word.clone(),
                    instruction: t.instruction.clone(),
                    weight: t.weight/nw.clone(),
                };

                transition_map.entry(b.clone()).or_insert_with(BinaryHeap::new).push(nt);
            }

        }

        let p = PushDownAutomaton {
            transitions: transition_map,
            initial: initial,
        };
        p.reduce_redundancy()
    }
}

impl<A, T, W> PushDownAutomaton<A, T, W>
    where A: Clone + Debug + Hash + Ord,
          T: Eq,
          W: Ord,
{
    pub fn list_transitions(&self) -> Vec<&Transition<PushDownInstruction<A>, T, W>> {
        let mut result = Vec::new();
        let mut keys: Vec<_> = self.transitions.keys().collect();

        keys.sort();

        for k in keys {
            if *k != self.initial.empty {
                for t in &self.transitions[k] {
                    result.push(t);
                }
            }
        }

        result
    }
}

impl<A> Instruction for PushDownInstruction<A>
    where A: Ord + PartialEq + Debug + Clone + Hash
{
    type Storage = PushDown<A>;

    fn apply(&self, p: PushDown<A>) -> Vec<PushDown<A>> {
        match *self {
            PushDownInstruction::Replace {ref current_val, ref new_val} =>
                p.replace(current_val, new_val).ok().into_iter().collect(),
            PushDownInstruction::ReplaceK {ref current_val, ref new_val, limit} =>
                p.replacek(current_val, new_val, limit).ok().into_iter().collect(),
        }
    }
}

impl<A, T, W> Automaton<PushDownInstruction<A>, T, W> for PushDownAutomaton<A, T, W>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          T: Clone + Debug + Eq + Hash + PartialOrd,
          W: One + Mul<Output=W> + Clone + Copy + Debug + Eq + Ord
{
    type Key = A;

    fn extract_key(c: &Configuration<PushDown<A>, T, W>) -> &A {
        if c.storage.is_bottom() {
            &c.storage.empty
        } else {
            c.storage.current_symbol()
        }
    }

    fn transitions(&self) -> &TransitionMap<A, T, W> {
        &self.transitions
    }

    fn initial(&self) -> PushDown<A> {
        self.initial.clone()
    }

    fn is_terminal(&self, c: &Configuration<PushDown<A>, T, W>) -> bool{
        c.word.is_empty() && c.storage.is_bottom()
    }
}

impl<A> PushDown<A>
{
    pub fn empty(&self) -> &A {
        &self.empty
    }
    pub fn current_symbol(&self) -> &A {
        self.elements.last().unwrap()
    }

    /// Checks whether stack is empty.
    pub fn is_bottom(&self) -> bool {
        self.elements.len() == 1
    }

    pub fn iter(&self) -> Iter<A> {
        self.elements.iter()
    }
}

impl<A> PushDown<A>
    where A: Clone
{
    pub fn from_vec(vec: Vec<A>) -> PushDown<A> {
        PushDown {
            empty: vec[0].clone(),
            elements: vec,
        }
    }

    /// New `PushDown<A>` with empty-symbol of type `A` and initial symbol of type `A`
    pub fn new(a: A, empty: A) -> PushDown<A> {
        PushDown::from_vec(vec![empty, a])
    }
}

impl<A> PushDown<A>
    where A: Clone + Ord
{
    /// Operations for Instructions:
    /// Replaces the uppermost elements with the given elements.
    /// TODO cur_sym ist given in reverse order.
    pub fn replace(mut self, cur_sym: &[A],  new_sym: &[A]) -> Result<Self, Self> {
        let mut new_cur_sym = cur_sym.to_vec(); //
        new_cur_sym.reverse();                  // TODO remove this

        if self.elements.ends_with(&new_cur_sym) {
            let n = self.elements.len();
            self.elements.truncate(n - cur_sym.len());
            self.elements.append(&mut new_sym.to_vec());
            Ok(self)
        } else {
            Err(self)
        }
    }

    /// Replaces uppermost element with the given elements.
    /// Truncates to the last `limit` elements.
    /// Also works without `cur_sym` if `is_bottom()`
    /// TODO cur_sym ist given in reverse order.
    pub fn replacek(mut self, cur_sym: &[A],  new_sym: &[A], limit: usize) -> Result<Self, Self> {
        let mut new_cur_sym = cur_sym.to_vec();        //
        new_cur_sym.truncate(self.elements.len() - 1); //
        new_cur_sym.reverse();                         // TODO remove this

        if self.elements.ends_with(&new_cur_sym) {
            let n = self.elements.len();
            self.elements.truncate(n - new_cur_sym.len());
            self.elements.append(&mut new_sym.to_vec());
            let m = self.elements.len();
            if m > limit + 1 {
                self.elements.drain(1 .. m - limit);
            }
            Ok(self)
        } else {
            Err(self)
        }
    }
}


impl<A> Display for PushDown<A>
    where A: Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        let mut iter1 = self.elements.iter().peekable();

        while let Some(nt) = iter1.next() {
            buffer.push_str(format!("{}", nt).as_str());
            if iter1.peek().is_some() {
                buffer.push_str(" ");
            }
        }
        write!(f, "stack: [{}], empty:{}", buffer, self.empty)
    }
}


impl<A> Display for PushDownInstruction<A>
    where A: Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PushDownInstruction::Replace { ref current_val, ref new_val} => {
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
            PushDownInstruction::ReplaceK { ref current_val, ref new_val , ref limit} => {
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

impl<A, T, W> Display for PushDownAutomaton<A, T, W>
    where A: Clone + Debug + Display + Hash + Ord,
          T: Display + Debug + Eq,
          W: Display + Debug + Ord,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut formatted_transitions = String::new();
        for t in self.list_transitions() {
            formatted_transitions.push_str(&t.to_string());
            formatted_transitions.push_str("\n");
        }
        write!(f, "initial: {}\n\n{}", self.initial.current_symbol(), formatted_transitions)
    }
}
