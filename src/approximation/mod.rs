use std::hash::Hash;
use std::fmt::Debug;
use std::ops::{Add, Mul, Div};
use num_traits::{Zero, One};

pub mod relabel;
pub mod ptk;

pub use approximation::relabel::*;

use automata;
pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

pub use self::relabel::*;
pub use self::ptk::*;

//functions that apply Strategys to Initial Configuration and Transitions
pub trait ApproximationStrategy<A1, A2, T1, T2> {
    fn approximate_initial(self, A1) -> A2;

    fn approximate_transition(self, T1) -> Vec<T2>;
}

//Approximates automaton using Strategy-Element
pub trait Approximation<T, O> {
    fn approximation(self, T) -> Result<O, String>;
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Add<f64, Output = f64> + Zero + One,
      S: Clone + ApproximationStrategy<PushDown<A>, PushDown<B>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
        automata::Transition<PushDown<B>, PushDownInstruction<B>, T, W>>>
      Approximation<S, PushDownAutomaton<B, T, W>> for PushDownAutomaton<A, T, W>
      where W : Add<Output = W>{

    fn approximation(self, strat : S) -> Result<PushDownAutomaton<B, T, W>, String>{
        let initial = strat.clone().approximate_initial(self.initial);

        let mut transitions = Vec::new();

        for (_, value) in self.transitions{
            for t in &value{
                let b = strat.clone().approximate_transition(t.clone()).clone();
                for l in b{
                    transitions.push(l);
                }
            }
        }

        Ok(PushDownAutomaton::new(
            transitions,
            initial,
            ))
    }
}
