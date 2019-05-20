use num_traits::Zero;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{AddAssign, MulAssign};

use crate::approximation::*;
use crate::automata::push_down_automaton::*;
use crate::automata::tree_stack_automaton::*;

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
where
    A: Clone + Hash + Ord,
    T: Clone + Eq + Hash + Ord,
    W: AddAssign + Copy + MulAssign + One + Ord + Zero,
{
    type I1 = TreeStackInstruction<A>;
    type I2 = PushDownInstruction<A>;
    type A1 = TreeStackAutomaton<A, T, W>;
    type A2 = PushDownAutomaton<A, T, W>;

    fn approximate_storage(&self, mut ts: TreeStack<A>) -> PushDown<A> {
        let mut pd = Vec::new();
        pd.push(ts.current_symbol().clone());

        while let Ok(smaller) = ts.down() {
            pd.push(smaller.current_symbol().clone());
            ts = smaller;
        }

        pd.reverse();
        PushDown::from(pd)
    }

    fn approximate_instruction(&self, instr: &TreeStackInstruction<A>) -> PushDownInstruction<A> {
        match *instr {
            TreeStackInstruction::Up {
                ref current_val,
                ref new_val,
                ..
            }
            | TreeStackInstruction::Push {
                ref current_val,
                ref new_val,
                ..
            } => PushDownInstruction::Replace {
                current_val: vec![current_val.clone()],
                new_val: vec![current_val.clone(), new_val.clone()],
            },
            TreeStackInstruction::Down {
                ref current_val,
                ref old_val,
                ref new_val,
            } => PushDownInstruction::Replace {
                current_val: vec![current_val.clone(), old_val.clone()],
                new_val: vec![new_val.clone()],
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_approximate_storage() {
        let mut ts = TreeStack::new('@');
        ts = ts.push(1, '1').unwrap();
        ts = ts.down().unwrap();
        ts = ts.push(2, '2').unwrap();
        ts = ts.push(2, '3').unwrap();
        ts = ts.down().unwrap();
        ts = ts.push(7, '4').unwrap();

        let tts = TTSElement::new();
        let control_pushdown = PushDown::from(vec!['@', '2', '4']);

        assert_eq!(
            control_pushdown,
            <TTSElement<_> as ApproximationStrategy<char, u8>>::approximate_storage(&tts, ts)
        );
    }

    #[test]
    fn test_approximate_instruction() {
        let tts = TTSElement::new();
        let inputs = vec![
            (
                TreeStackInstruction::Up {
                    n: 2,
                    current_val: '@',
                    old_val: '2',
                    new_val: '3',
                },
                PushDownInstruction::Replace {
                    current_val: vec!['@'],
                    new_val: vec!['@', '3'],
                },
            ),
            (
                TreeStackInstruction::Down {
                    current_val: '4',
                    old_val: '2',
                    new_val: '3',
                },
                PushDownInstruction::Replace {
                    current_val: vec!['4', '2'],
                    new_val: vec!['3'],
                },
            ),
            (
                TreeStackInstruction::Push {
                    n: 2,
                    current_val: '2',
                    new_val: '3',
                },
                PushDownInstruction::Replace {
                    current_val: vec!['2'],
                    new_val: vec!['2', '3'],
                },
            ),
        ];

        for (ts_instruction, pd_control_instruction) in inputs {
            assert_eq!(
                pd_control_instruction,
                <TTSElement<_> as ApproximationStrategy<char, u8>>::approximate_instruction(
                    &tts,
                    &ts_instruction,
                )
            );
        }
    }
}
