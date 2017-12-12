use num_traits::Zero;
use std::marker::PhantomData;
use std::ops::AddAssign;

use approximation::*;
use tree_stack_automaton::*;
use push_down_automaton::*;

/// `ApproximationStrategy` that approximates a `TreeStackAutomaton` into a `PushDownAutomaton`
#[derive(Clone, Debug)]
pub struct TTSElement<A> {
    _dummy: PhantomData<A>,
}

impl<A> TTSElement<A> {
    pub fn new() -> Self {
        TTSElement {
            _dummy: PhantomData,
        }
    }
}

impl<A, T, W> ApproximationStrategy<T, W> for TTSElement<A>
    where A: Clone + Debug + Hash + Ord,
          T: Clone + Debug + Eq + Hash + PartialOrd,
          W: AddAssign + Copy + Debug + One + Ord + Zero,
{
    type I1 = TreeStackInstruction<A>;
    type I2 = PushDownInstruction<A>;
    type A1 = TreeStackAutomaton<A, T, W>;
    type A2 = PushDownAutomaton<A, T, W>;

    fn approximate_storage(&self, a: TreeStack<A>)-> PushDown<A> {
        let mut pd = Vec::new();
        pd.push(a.current_symbol().clone());
        loop {
            if let Ok(a) = a.clone().down() {
                pd.push(a.current_symbol().clone());
            } else {
                break;
            }
        }
        pd.reverse();
        PushDown::from_vec(pd)
    }

    fn approximate_instruction(&self, instr: &TreeStackInstruction<A>)
                               -> PushDownInstruction<A>
    {
        match *instr {
            TreeStackInstruction::Up { ref current_val, ref new_val, ..}
            | TreeStackInstruction::Push { ref current_val, ref new_val, .. } => {
                PushDownInstruction::Replace {
                    current_val: vec![current_val.clone()],
                    new_val: vec![current_val.clone(), new_val.clone()],
                }
            },
            TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                PushDownInstruction::Replace {
                    current_val: vec![current_val.clone(), old_val.clone()],
                    new_val: vec![new_val.clone()],
                }
            },
        }
    }

}
