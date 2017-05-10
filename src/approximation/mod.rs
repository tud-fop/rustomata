use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

use automata;
pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

pub use self::relabel::*;

///Approximation strategy of configuration of type to type `C`
pub enum ApproximationStrategy {
    Relab,
    Strat2,
    Strat3,
}

/// Approximation of self via ApproximationStrategy `T`
pub trait Approximation<T, P, N1, N2, O> {
    fn approximation(self, T, P) -> Result<O, String>;
}

pub trait Relabel<P, N1, N2, O>{
    fn relabel(&self, P) -> O;
}

impl <A: Ord + PartialEq + Debug + Clone + Hash +  Relabel<P, N1, N2, B>,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone,
      W: Ord + Eq + Clone,
      P: Copy, N1: Clone, N2: Clone> Approximation<ApproximationStrategy, P, N1, N2, PushDownAutomaton<B, T, W>> for PushDownAutomaton<A, T, W>
    where P: Fn(N1) -> N2{

    fn approximation(self, strat : ApproximationStrategy, func : P) -> Result<PushDownAutomaton<B, T, W>, String>{
        let mut transitions = Vec::new();

        match strat{
            ApproximationStrategy::Relab => {
                let initial = self.initial.relabel(func);
                for (_,t) in self.transitions{
                    for t2 in t{
                        match t2.instruction{
                            PushDownInstruction::Replace {ref current_val, ref new_val} => {
                                let mut st = Vec::new();
                                for nt in new_val{
                                    st.push(nt.relabel(func));
                                }
                                transitions.push(
                                    automata::Transition {
                                        _dummy: PhantomData,
                                        word: t2.word.clone(),
                                        weight: t2.weight.clone(),
                                        instruction: PushDownInstruction::Replace {
                                            current_val: current_val.relabel(func),
                                            new_val: st.clone(),
                                        }
                                    }
                                );
                            }
                            PushDownInstruction::Pop {ref current_val} => {
                                transitions.push(
                                    automata::Transition {
                                        _dummy: PhantomData,
                                        word: t2.word.clone(),
                                        weight: t2.weight.clone(),
                                        instruction: PushDownInstruction::Pop {
                                            current_val: current_val.relabel(func),
                                        }
                                    }
                                );
                            }
                        }
                    }


                }


                Ok(PushDownAutomaton::new(
                    transitions,
                    initial,
                ))

            },
            ApproximationStrategy::Strat2 => return Err("strategy not implemented (yet?)".to_string()),
            ApproximationStrategy::Strat3 => return Err("strategy not implemented (yet?)".to_string()),
        }

    }
}
