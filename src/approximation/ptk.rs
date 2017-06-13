use std::marker::PhantomData;

pub use automata::*;
pub use approximation::*;

pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

//Strategy Element for mapping pushdown to its top most k elements
#[derive(Clone)]
pub struct PDTopKElement<A>{
    pub dummy: PhantomData<A>,
    pub size: usize,
}

impl <A : Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Add<f64, Output = f64> + Zero + One> ApproximationStrategy<PushDown<A>, PushDown<A>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>
      for PDTopKElement<A>{

    fn approximate_initial(self, a : PushDown<A>)-> PushDown<A>{
        let mut b = a.elements.clone();
        b.remove(0);
        let pushdown = PushDown::new(b[0].clone(), a.empty.clone());
        let ps=pushdown.replacek(&b[0], &b, &self.size.clone());
        ps[0].clone()


    }

    fn approximate_transition(self, t :  automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>) ->
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>{
        match t.instruction{
            PushDownInstruction::Replace {ref current_val, ref new_val} => {
                automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::ReplaceK {
                        current_val: current_val.clone(),
                        new_val: new_val.clone(),
                        limit: self.size.clone(),
                    }
                }
            },
            PushDownInstruction::ReplaceK {ref current_val, ref new_val, ..} => {
                automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::ReplaceK {
                        current_val: current_val.clone(),
                        new_val: new_val.clone(),
                        limit: self.size.clone(),
                    }
                }
            },
        }
    }
}
