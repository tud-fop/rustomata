extern crate num_traits;

use std::collections::{BinaryHeap, HashMap};
use std::fmt;
use std::hash::Hash;
use std::ops::Mul;
use std::vec::Vec;
use num_traits::One;

use automata::{Automaton, Configuration, Transition};

mod from_pmcfg;
mod from_str;
mod tree_stack;
mod tree_stack_instruction;

pub use self::from_pmcfg::*;
pub use self::tree_stack::*;
pub use self::tree_stack_instruction::*;


/// Automaton with storage type `TreeStack<A>`, terminals of type `T` and weights of type `W`.
#[derive(Debug, Clone)]
pub struct TreeStackAutomaton<A: Ord + PartialEq + fmt::Debug + Clone + Hash, T: Eq, W: Ord + Eq> {
    pub transitions: HashMap<A, BinaryHeap<Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>>,
    pub initial: TreeStack<A>,
}


impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash, T: Eq, W: Ord + Eq> TreeStackAutomaton<A, T, W> {
    pub fn new(transitions: Vec<Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>, initial: TreeStack<A>)
               -> TreeStackAutomaton<A, T, W> {
        let mut transition_map: HashMap<A, BinaryHeap<Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>>  = HashMap::new();

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

    pub fn list_transitions(&self) -> Vec<&Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>> {
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


impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash,
     T: Clone + fmt::Debug + Eq + Hash,
     W: One + Mul<Output=W> + Clone + Copy + fmt::Debug + Eq + Ord>
    Automaton<TreeStack<A>, TreeStackInstruction<A>, T, W> for TreeStackAutomaton<A, T, W> {
        type Key = A;

        fn extract_key(c: &Configuration<TreeStack<A>, T, W>) -> &A {
            c.storage.current_symbol()
        }

        fn transitions(&self) -> &HashMap<A, BinaryHeap<Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>> {
            &self.transitions
        }

        fn initial(&self) -> TreeStack<A> {
            self.initial.clone()
        }

        fn is_terminal(&self, c: &Configuration<TreeStack<A>, T, W>) -> bool{
            c.word.is_empty() && c.storage.is_at_bottom()
        }
    }

impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash + fmt::Display,
     T: Clone + fmt::Debug + Eq + Hash,
     W: One + Mul<Output=W> + Clone + Copy + fmt::Debug + Eq + Ord + fmt::Display>
    fmt::Display for TreeStackAutomaton<A, T, W> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let mut formatted_transitions = String::new();
            for t in self.list_transitions() {
                formatted_transitions.push_str(&t.to_string());
                formatted_transitions.push_str("\n");
            }
            write!(f, "initial: {}\n\n{}", self.initial.current_symbol(), formatted_transitions)
        }
    }


