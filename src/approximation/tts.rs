use std::marker::PhantomData;

pub use automata::*;
pub use approximation::*;

pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

//Strategy Element for mapping pushdown to its top most k elements
#[derive(Clone, Debug)]
pub struct TTSElement<A>{
    pub dummy: PhantomData<A>,
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Add<f64, Output = f64> + Zero + One> ApproximationStrategy<TreeStack<PosState<A>>,PushDown<PosState<A>>,
        automata::Transition<TreeStack<PosState<A>>, TreeStackInstruction<PosState<A>>, T, W>,
        automata::Transition<PushDown<PosState<A>>,  PushDownInstruction<PosState<A>>, T, W>>
      for TTSElement<A>{

    fn approximate_initial(&self, a : TreeStack<PosState<A>>)-> PushDown<PosState<A>>{
        let np = Vec::new();
        let nempty = a.tree.get(&np).unwrap();
        let ele = vec![nempty.clone()];

        let p = PushDown{
            elements: ele.clone(),
            empty: ele[0].clone(),
        };
        p
    }

    fn approximate_transition(&self, t :  automata::Transition<TreeStack<PosState<A>>, TreeStackInstruction<PosState<A>>, T, W>) ->
        automata::Transition<PushDown<PosState<A>>, PushDownInstruction<PosState<A>>, T, W>{
        match t.instruction{
            TreeStackInstruction::Up { ref current_val, ref new_val, ..} => {
                automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![current_val.clone()],
                        new_val: vec![current_val.clone(), new_val.clone()],
                    }
                }
            }
            TreeStackInstruction::Push { ref current_val, ref new_val, .. } => {
                automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![current_val.clone()],
                        new_val: vec![current_val.clone(), new_val.clone()],
                    }
                }
            }
            TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![current_val.clone(), old_val.clone()],
                        new_val: vec![new_val.clone()],
                    }
                }
            }
        }
    }
}
