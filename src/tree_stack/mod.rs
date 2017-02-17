extern crate num_traits;

use std::collections::{BinaryHeap, HashMap};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Mul;
use std::vec::Vec;
use num_traits::One;

use automata;

pub mod from_pmcfg;
mod from_str;

pub use from_pmcfg::*;

/// Automaton with storage type `TreeStack<A>`, terminals of type `T` and weights of type `W`.
#[derive(Debug, Clone)]
pub struct TreeStackAutomaton<A: Ord + PartialEq + Debug + Clone + Hash, T: Eq, W: Ord + Eq> {
    pub transitions: HashMap<A, BinaryHeap<automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>>,
    pub initial: TreeStack<A>,
}


/// Instruction on `TreeStack<A>`s.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TreeStackInstruction<A> {
    Up {
        n: u8,
        current_val: A,
        old_val: A,
        new_val: A,
    },
    Push { n: u8, current_val: A, new_val: A },
    Down {
        current_val: A,
        old_val: A,
        new_val: A,
    },
}


/// Upside-down tree with designated position and node labels of type `A`.
/// We always assume that `self.contains_key(pointer) == true`.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TreeStack<A: Ord> {
    tree: HashMap<Vec<u8>, A>,
    pointer: Vec<u8>,
}


impl<A: Ord + PartialEq + Debug + Clone + Hash, T: Eq, W: Ord + Eq> TreeStackAutomaton<A, T, W> {
    pub fn new(transitions: Vec<automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>, initial: TreeStack<A>)
               -> TreeStackAutomaton<A, T, W> {
        let mut transition_map: HashMap<A, BinaryHeap<automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>>  = HashMap::new();

        for t in transitions {
            let a =
                match t.instruction {
                    TreeStackInstruction::Up   { ref current_val, .. } => current_val.clone(),
                    TreeStackInstruction::Push { ref current_val, .. } => current_val.clone(),
                    TreeStackInstruction::Down { ref current_val, .. } => current_val.clone()
                };

            if !transition_map.contains_key(&a) {
                transition_map.insert(a.clone(), BinaryHeap::new());
                ()
            }

            transition_map.get_mut(&a).unwrap().push(t);
        }

        TreeStackAutomaton {
            transitions: transition_map,
            initial: initial,
        }
    }

    pub fn list_transitions(&self) -> Vec<&automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>> {
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


impl<A: Ord + PartialEq + Debug + Clone + Hash> automata::Instruction<TreeStack<A>>
    for TreeStackInstruction<A> {
    fn apply(&self, t: TreeStack<A>) -> Option<TreeStack<A>> {
        match self {
            &TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } => {
                t.up(n, current_val, Some(&old_val), new_val)
            }
            &TreeStackInstruction::Push { n, ref current_val, ref new_val } => {
                t.up(n, current_val, None, new_val)
            }
            &TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                t.down(current_val, old_val, new_val)
            }
        }
    }
}


impl<A: Ord + PartialEq + Debug + Clone + Hash,
     T: Clone + Debug + Eq + Hash,
     W: One + Mul<Output=W> + Clone + Copy + Debug + Eq + Ord>
    automata::Automaton<TreeStack<A>, TreeStackInstruction<A>, T, W> for TreeStackAutomaton<A, T, W> {
        type Key = A;

        fn extract_key(c: &automata::Configuration<TreeStack<A>, T, W>) -> &A {
            c.storage.current_symbol()
        }

        fn transitions(&self) -> &HashMap<A, BinaryHeap<automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>> {
            &self.transitions
        }

        fn initial(&self) -> TreeStack<A> {
            self.initial.clone()
        }

        fn is_terminal(&self, c: &automata::Configuration<TreeStack<A>, T, W>) -> bool{
            c.word.is_empty() && c.storage.pointer.is_empty()
        }
    }

impl<A: Ord + PartialEq + Clone + Debug> TreeStack<A> {
    /// Creates a new `TreeStack<A>` with root label `a`.
    pub fn new(a: A) -> TreeStack<A> {
        let mut tree: HashMap<Vec<u8>, A> = HashMap::new();
        tree.insert(Vec::new(), a);
        TreeStack {
            tree: tree,
            pointer: Vec::new(),
        }
    }

    pub fn current_symbol(&self) -> &A {
        self.tree.get(&self.pointer).unwrap()
    }

    /// Returns 'None' if the current node is different from `current_val`.
    /// If `old_val == None` then this method checks if the `n`th child is vacant and pushes `new_val` there.
    /// If `old_val == Some(x)` then this method checks if the `n`th child is `old_val` and goes there, replacing `old_val` by `new_val`.
    pub fn up(&self,
              n: u8,
              current_val: &A,
              old_val: Option<&A>,
              new_val: &A)
              -> Option<TreeStack<A>> {
        let mut new_pointer = self.pointer.clone();
        new_pointer.push(n);

        match (self.tree.get(&self.pointer), self.tree.get(&new_pointer).clone()) {
            (Some(val), o_val) if val == current_val && o_val == old_val => {
                let mut new_tree = self.tree.clone();
                new_tree.insert(new_pointer.clone(), new_val.clone());
                Some(TreeStack {
                    tree: new_tree,
                    pointer: new_pointer,
                })
            }
            _ => None,
        }
    }

    /// Moves the `pointer` to the parent node if one exists and returns `None` otherwise.
    pub fn down(&self, current_val: &A, old_val: &A, new_val: &A) -> Option<TreeStack<A>> {
        if self.pointer.is_empty() {
            None
        } else {
            let mut new_pointer = self.pointer.clone();
            new_pointer.pop();
            match (self.tree.get(&self.pointer), self.tree.get(&new_pointer)) {
                (Some(c_val), Some(o_val)) if c_val == current_val && o_val == old_val => {
                    let mut new_tree = self.tree.clone();
                    new_tree.insert(new_pointer.clone(), new_val.clone());
                    Some(TreeStack {
                        tree: new_tree,
                        pointer: new_pointer,
                    })
                }
                _ => None,
            }
        }
    }
}
