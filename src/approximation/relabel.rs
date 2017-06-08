use std::marker::PhantomData;

use automata;
pub use approximation::*;
pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

// relabel function for configurations and states
pub trait Relabel<N1, N2, O>{
    fn relabel(&self, fn(N1)-> N2) -> O;
}

//Strategy Element for Relabel
#[derive(Clone)]
pub struct RlbElement<A, N1, N2>{
    pub dummy: PhantomData<A>,
    pub func: fn(N1)-> N2
}

impl <A1 : Ord + PartialEq + Debug + Clone + Hash + Relabel<N1, N2, A2>,
      A2:  Ord + PartialEq + Debug + Clone + Hash,
      N1: Clone, N2: Clone,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Add<f64, Output = f64> + Zero + One> ApproximationStrategy<PushDown<A1>, PushDown<A2>,
        automata::Transition<PushDown<A1>, PushDownInstruction<A1>, T, W>,
        automata::Transition<PushDown<A2>, PushDownInstruction<A2>, T, W>>
      for RlbElement<PushDown<A1>, N1, N2>{
    fn approximate_initial(self, a : PushDown<A1>)-> PushDown<A2>{
        a.relabel(self.func)
    }

    fn approximate_transition(self, t :  automata::Transition<PushDown<A1>, PushDownInstruction<A1>, T, W>) ->
        automata::Transition<PushDown<A2>, PushDownInstruction<A2>, T, W>{
        match t.instruction{
            PushDownInstruction::Replace {ref current_val, ref new_val, ref limit} => {
                let mut st = Vec::new();
                for nt in new_val{
                    st.push(nt.relabel(self.func));
                }
                automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: current_val.relabel(self.func),
                        new_val: st.clone(),
                        limit: limit.clone(),
                    }
                }
            }
        }
    }
}
