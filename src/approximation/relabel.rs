use approximation::*;
use num_traits::Zero;
use push_down_automaton::*;
use std::ops::AddAssign;

/// `ApproximationStrategy` that uses the `Relabel` trait to relabel internal values via a `EquivalenceClass`
pub struct RlbElement<'a, A1, A2>
    where A1: 'a,
          A2: 'a,
{
    pub mapping: &'a Fn(&A1) -> A2
}

impl<'a, A1, A2> RlbElement<'a, A1, A2> {
    pub fn new(mapping: &'a Fn(&A1) -> A2) -> Self {
        RlbElement {
            mapping: mapping,
        }
    }
}

impl<'a, A1, A2, T, W> ApproximationStrategy<T, W> for RlbElement<'a, A1, A2>
    where A1: Clone + Hash + Ord,
          A2: Clone + Hash + Ord,
          T: Clone + Eq + Hash + Ord,
          W: AddAssign + Copy + MulAssign + One + Ord + Zero,
{
    type I1 = PushDownInstruction<A1>;
    type I2 = PushDownInstruction<A2>;
    type A1 = PushDownAutomaton<A1, T, W>;
    type A2 = PushDownAutomaton<A2, T, W>;


    fn approximate_storage(&self, pd: PushDown<A1>)-> PushDown<A2> {
        pd.map(&self.mapping)
    }

    fn approximate_instruction(&self, instr: &PushDownInstruction<A1>)
                               -> PushDownInstruction<A2>
    {
        match *instr {
            PushDownInstruction::Replace { ref current_val, ref new_val } => {
                PushDownInstruction::Replace {
                    current_val: current_val.iter().map(self.mapping).collect(),
                    new_val: new_val.iter().map(self.mapping).collect(),
                }
            },
            PushDownInstruction::ReplaceK { ref current_val, ref new_val, limit} => {
                PushDownInstruction::ReplaceK {
                    current_val: current_val.iter().map(self.mapping).collect(),
                    new_val: new_val.iter().map(self.mapping).collect(),
                    limit: limit,
                }
            },
        }
    }
}
