use std::marker::PhantomData;

pub use automata::*;
pub use approximation::*;

pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

//Strategy Element for mapping pushdown to its top most k elements
#[derive(Clone)]
pub struct TTSElement<A>{
    pub dummy: PhantomData<A>,
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Add<f64, Output = f64> + Zero + One> ApproximationStrategy<TreeStack<PosState<A>>,PushDown<PosState<A>>,
        automata::Transition<TreeStack<PosState<A>>, TreeStackInstruction<PosState<A>>, T, W>,
        automata::Transition<PushDown<PosState<A>>,  PushDownInstruction<PosState<A>>, T, W>>
      for TTSElement<A>{

    fn approximate_initial(self, a : TreeStack<PosState<A>>)-> PushDown<PosState<A>>{
        let mut np = Vec::new();
        let nempty = a.tree.get(&np).unwrap();
        let mut ele = vec![nempty.clone()];

        for p in a.pointer{
            np.push(p);
            ele.push(a.tree.get(&np).unwrap().clone());
        }

        PushDown{
            elements: ele,
            empty: PosState::Designated,
        }
    }

    fn approximate_transition(self, t :  automata::Transition<TreeStack<PosState<A>>, TreeStackInstruction<PosState<A>>, T, W>) ->
        automata::Transition<PushDown<PosState<A>>, PushDownInstruction<PosState<A>>, T, W>{
        match t.instruction{
            TreeStackInstruction::Up { ref current_val, ref new_val, ..} => {
                match current_val{
                    &PosState::Position(ref a, i, j) =>{
                        let mut nv = Vec::new();
                        nv.push(PosState::Position(a.clone(), i, j+1));
                        nv.push(new_val.clone());
                        automata::Transition {
                            _dummy: PhantomData,
                            word: t.word.clone(),
                            weight: t.weight.clone(),
                            instruction: PushDownInstruction::Replace {
                                current_val: current_val.clone(),
                                new_val: nv,
                            }
                        }
                    },
                    &PosState::Designated => {
                        let mut nv = Vec::new();
                        nv.push(PosState::Designated);
                        nv.push(new_val.clone());
                        automata::Transition {
                            _dummy: PhantomData,
                            word: t.word.clone(),
                            weight: t.weight.clone(),
                            instruction: PushDownInstruction::Replace {
                                current_val: current_val.clone(),
                                new_val: nv,
                            }
                        }
                    },
                }
            }
            TreeStackInstruction::Push { ref current_val, ref new_val, .. } => {
                match current_val{
                    &PosState::Position(ref a, i, j) =>{
                        let mut nv = Vec::new();
                        nv.push(PosState::Position(a.clone(), i, j+1));
                        nv.push(new_val.clone());
                        automata::Transition {
                            _dummy: PhantomData,
                            word: t.word.clone(),
                            weight: t.weight.clone(),
                            instruction: PushDownInstruction::Replace {
                                current_val: current_val.clone(),
                                new_val: nv,
                            }
                        }
                    },
                    &PosState::Designated => {
                        let mut nv = Vec::new();
                        nv.push(PosState::Designated);
                        nv.push(new_val.clone());
                        automata::Transition {
                            _dummy: PhantomData,
                            word: t.word.clone(),
                            weight: t.weight.clone(),
                            instruction: PushDownInstruction::Replace {
                                current_val: current_val.clone(),
                                new_val: nv,
                            }
                        }
                    },
                }
            }
            TreeStackInstruction::Down { ref current_val, .. } => {
                automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: current_val.clone(),
                        new_val: Vec::new(),
                    }
                }
            }
        }
    }
}
