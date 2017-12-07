use approximation::*;
pub use approximation::equivalence_classes::EquivalenceClass;
use push_down_automaton::*;

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

impl<'a, A1, A2> ApproximationStrategy for RlbElement<'a, A1, A2>
    where A1: Clone + Debug + Hash + Ord,
          A2: Clone + Debug + Hash + Ord,
{
    type I1 = PushDownInstruction<A1>;
    type I2 = PushDownInstruction<A2>;

    fn approximate_storage(&self, pd: PushDown<A1>)-> PushDown<A2> {
        pd.map(self.mapping)
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
