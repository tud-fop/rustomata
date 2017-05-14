use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Add;

use automata;
pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

pub use self::relabel::*;

#[derive(Clone)]
pub struct RlbElement<A, N1, N2>{
    pub dummy: PhantomData<A>,
    pub func: fn(N1)-> N2

}

pub trait ApproximationStrategy<A1, A2, T1, T2> {
    fn approximate_initial(self, A1) -> A2;

    fn approximate_transition(self, T1) -> T2;
}

pub trait Approximation<T, O> {
    fn approximation(self, T) -> Result<O, String>;
}

pub trait Relabel<N1, N2, O>{
    fn relabel(&self, fn(N1)-> N2) -> O;
}

impl <A1 : Ord + PartialEq + Debug + Clone + Hash + Relabel<N1, N2, A2>,
      A2:  Ord + PartialEq + Debug + Clone + Hash,
      N1: Clone, N2: Clone,
      T: Eq + Clone,
      W: Ord + Eq + Clone + Add,> ApproximationStrategy<PushDown<A1>, PushDown<A2>,
        automata::Transition<PushDown<A1>, PushDownInstruction<A1>, T, W>,
        automata::Transition<PushDown<A2>, PushDownInstruction<A2>, T, W>>
      for RlbElement<PushDown<A1>, N1, N2>{
    fn approximate_initial(self, a : PushDown<A1>)-> PushDown<A2>{
        a.relabel(self.func)
    }

    fn approximate_transition(self, t :  automata::Transition<PushDown<A1>, PushDownInstruction<A1>, T, W>) ->
        automata::Transition<PushDown<A2>, PushDownInstruction<A2>, T, W>{
        match t.instruction{
            PushDownInstruction::Replace {ref current_val, ref new_val} => {
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
                    }
                }
            }
            PushDownInstruction::Pop {ref current_val} => {
                automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Pop {
                        current_val: current_val.relabel(self.func),
                    }
                }
            }
        }
    }
}





impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone,
      W: Ord + Eq + Clone + Add,
      S: Clone + ApproximationStrategy<PushDown<A>, PushDown<B>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
        automata::Transition<PushDown<B>, PushDownInstruction<B>, T, W>>>
      Approximation<S, PushDownAutomaton<B, T, W>> for PushDownAutomaton<A, T, W>
      where W : Add<Output = W>{

    fn approximation(self, strat : S) -> Result<PushDownAutomaton<B, T, W>, String>{
        let initial = strat.clone().approximate_initial(self.initial);

        let mut transitions = Vec::new();
        let mut stransitions = Vec::new();

        for (_, value) in self.transitions{
            for t in &value{
                transitions.push(strat.clone().approximate_transition(t.clone()));
            }
        }
        //maps equal transitions onto each other
        for t in transitions{
            let mut s = false;
            let mut r = None;
            let mut t3 = t.clone();
            let lword = t.word.clone();
            let linst = t.instruction.clone();
            stransitions.push(t.clone());
            for i in 0..stransitions.clone().len(){
                let st = stransitions[i].clone();
                if st.word.clone() == lword && st.instruction.clone() == linst{
                    if !(st.clone() == *stransitions.iter().last().unwrap()){
                        s = true;
                        r = Some(i);
                        t3 = automata::Transition{
                            _dummy: PhantomData,
                            word: st.word.clone(),
                            weight: st.weight.clone()+t.weight.clone(),
                            instruction: st.instruction.clone(),
                        }
                    }
                }
            }
            if s{
                stransitions.remove(r.unwrap());
                stransitions.pop();
                stransitions.push(t3);
            }
        }



        Ok(PushDownAutomaton::new(
            stransitions,
            initial,
            ))
    }
}
