use num_traits::Zero;
use std::marker::PhantomData;
use std::ops::AddAssign;

use approximation::*;
use push_down_automaton::*;

/// `ApproximationStrategy`that limits a `PushDownAutomaton` to a certain height.
#[derive(Clone, Debug)]
pub struct PDTopKElement<A> {
    _dummy: PhantomData<A>,
    pub size: usize,
}

impl<A> PDTopKElement<A> {
    pub fn new(size: usize) -> Self {
        assert!(size >= 1);
        PDTopKElement{
            _dummy: PhantomData,
            size: size,
        }
    }
}

impl<A, T, W> ApproximationStrategy<T, W> for PDTopKElement<A>
    where A: Clone + Debug + Ord + Hash,
          T: Clone + Debug + Eq + Hash + PartialOrd,
          W: AddAssign + Copy + Debug + One + Ord + Zero,
{
    type I1 = PushDownInstruction<A>;
    type I2 = PushDownInstruction<A>;
    type A1 = PushDownAutomaton<A, T, W>;
    type A2 = PushDownAutomaton<A, T, W>;

    fn approximate_storage(&self, a: PushDown<A>) -> PushDown<A> {
        let new_empty = a.empty().clone();
        let mut new_elements: Vec<_> = a.iter().cloned().rev().take(self.size - 1).collect();
        new_elements.push(new_empty);
        new_elements.reverse();
        PushDown::from_vec(new_elements)
    }

    fn approximate_instruction(&self, instr: &PushDownInstruction<A>)
                               -> PushDownInstruction<A>
    {
        match *instr {
            PushDownInstruction::Replace { ref current_val, ref new_val }
            | PushDownInstruction::ReplaceK { ref current_val, ref new_val, .. } => {
                PushDownInstruction::ReplaceK {
                    current_val: current_val.clone(),
                    new_val: new_val.clone(),
                    limit: self.size,
                }
            },
        }
    }
}
