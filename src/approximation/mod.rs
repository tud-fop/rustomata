use std::hash::Hash;
use std::fmt::Debug;
use std::ops::{Add, Mul, Div};
use num_traits::{Zero, One};

pub mod relabel;
pub mod ptk;
pub mod tts;

pub use approximation::relabel::*;

use automata;
pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

pub use self::relabel::*;
pub use self::ptk::*;
pub use self::tts::*;

//functions that apply Strategys to Initial Configuration and Transitions
pub trait ApproximationStrategy<A1, A2, T1, T2> {
    fn approximate_initial(&self, A1) -> A2;

    fn approximate_transition(&self, T1) -> T2;
}

//Approximates automaton using Strategy-Element
pub trait Approximation<T, O> {
    fn approximation(&self, &T) -> Result<O, String>;
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Zero + One,
      S: ApproximationStrategy<PushDown<A>, PushDown<B>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
        automata::Transition<PushDown<B>, PushDownInstruction<B>, T, W>>>
      Approximation<S, PushDownAutomaton<B, T, W>> for PushDownAutomaton<A, T, W>
      where W : Add<Output = W>{

    fn approximation(&self, strat : &S) -> Result<PushDownAutomaton<B, T, W>, String>{
        let initial = strat.approximate_initial(self.initial.clone());

        let mut transitions = Vec::new();

        for (_, value) in self.transitions.clone(){
            for t in &value{
                let b = strat.approximate_transition(t.clone());
                transitions.push(b);
            }
        }

        Ok(PushDownAutomaton::new(
            transitions,
            initial,
            ))
    }
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Zero + One,
      S: Clone + ApproximationStrategy<TreeStack<A>, PushDown<B>,
        automata::Transition<TreeStack<A>,TreeStackInstruction<A>, T, W>,
        automata::Transition<PushDown<B>, PushDownInstruction<B>, T, W>>>
      Approximation<S, PushDownAutomaton<B, T, W>> for TreeStackAutomaton<A, T, W>
      where W : Add<Output = W>{

    fn approximation(&self, strat : &S) -> Result<PushDownAutomaton<B, T, W>, String>{
        let initial1 = strat.approximate_initial(self.initial.clone());
        let i = self.initial.tree.get(&Vec::new()).unwrap();
        let mut fina = initial1.empty.clone();

        let mut transitions = Vec::new();

        for (_, value) in self.transitions.clone(){
            for t in &value{
                match t.instruction{
                    TreeStackInstruction::Down { ref old_val, .. }=>{
                        let b = strat.approximate_transition(t.clone());
                        transitions.push(b.clone());
                        if *old_val == *i{
                            match b.instruction{
                                PushDownInstruction::Replace {ref new_val, ..} =>{
                                    fina = new_val[0].clone();
                                },
                                _=>(),
                            }
                        }
                    },
                    _=> {
                        let b = strat.approximate_transition(t.clone());
                        transitions.push(b);
                    },
                }

            }
        }
        let initial2 = PushDown::new(initial1.empty, fina);

        Ok(PushDownAutomaton::new(
            transitions,
            initial2
            ))
    }
}
