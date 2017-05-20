use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Add, Mul, Div};
use num_traits::{One, Zero};

pub mod relabel;

pub use approximation::relabel::*;

use automata;
pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

pub use self::relabel::*;


//Strategy Element for Relabel
#[derive(Clone)]
pub struct RlbElement<A, N1, N2>{
    pub dummy: PhantomData<A>,
    pub func: fn(N1)-> N2

}

#[derive(Clone)]
pub struct PDTopKElement<A>{
    pub dummy: PhantomData<A>,
    pub size: i64,
}

//functions that apply Strategys to Initial Configuration and Transitions
pub trait ApproximationStrategy<A1, A2, T1, T2> {
    fn approximate_initial(self, A1) -> A2;

    fn approximate_transition(self, T1) -> Vec<T2>
}

//Approximates automaton using Strategy-Element
pub trait Approximation<T, O> {
    fn approximation(self, T) -> Result<O, String>;
}



impl <A1 : Ord + PartialEq + Debug + Clone + Hash + Relabel<N1, N2, A2>,
      A2:  Ord + PartialEq + Debug + Clone + Hash,
      N1: Clone, N2: Clone,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Zero + One> ApproximationStrategy<PushDown<A1>, PushDown<A2>,
        automata::Transition<PushDown<A1>, PushDownInstruction<A1>, T, W>,
        automata::Transition<PushDown<A2>, PushDownInstruction<A2>, T, W>>
      for RlbElement<PushDown<A1>, N1, N2>{
    fn approximate_initial(self, a : PushDown<A1>)-> PushDown<A2>{
        a.relabel(self.func)
    }

    fn approximate_transition(self, t :  automata::Transition<PushDown<A1>, PushDownInstruction<A1>, T, W>) ->
        Vec<automata::Transition<PushDown<A2>, PushDownInstruction<A2>, T, W>>{
        match t.instruction{
            PushDownInstruction::Replace {ref current_val, ref new_val} => {
                let mut st = Vec::new();
                for nt in new_val{
                    st.push(nt.relabel(self.func));
                }
                let mut b = Vec::new();
                b.push(automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: current_val.relabel(self.func),
                        new_val: st.clone(),
                    }
                });
                b
            }
            PushDownInstruction::Pop {ref current_val} => {
                let mut b = Vec::new();
                b.push(automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Pop {
                        current_val: current_val.relabel(self.func),
                    }
                });
                b
            }
        }
    }
}

/*
impl <A1 : Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Zero + One> ApproximationStrategy<PushDown<A>, PushDown<A>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>
      for PDTopKElement<PushDown<A>>{
    fn approximate_initial(self, a : PushDown<A>)-> PushDown<A>{
        let mut b = a.elements.clone();
        let pushdown = PushDown{
            elements: b[1].clone(),
            limit: self.size,
            empty: a.empty.clone(),
        }
        pushdown.replace(b[1].clone(),b.clone()).unwrap()

    }

    fn approximate_transition(self, t :  automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>) ->
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>{
        match t.instruction{
            PushDownInstruction::Replace {ref current_val, ref new_val} => {
                let mut st = Vec::new();
                for nt in new_val{
                    st.push(nt.relabel(self.func));
                }
                let mut b = Vec::new();
                b.push(automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: current_val.relabel(self.func),
                        new_val: st.clone(),
                    }
                });
                b
            }
            PushDownInstruction::Pop {ref current_val} => {
                let mut b = Vec::new();
                b.push(automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Pop {
                        current_val: current_val.relabel(self.func),
                    }
                });
                b
            }
        }
    }
}
*/



impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Zero + One,
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
                let mut b = strat.clone().approximate_transition(t.clone()).clone();
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
