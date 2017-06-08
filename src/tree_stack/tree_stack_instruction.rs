use std::fmt;
use std::hash::Hash;

use automata::Instruction;
use tree_stack::TreeStack;


/// Instruction on `TreeStack<A>`s.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TreeStackInstruction<A> {
    Up {
        n: u8,
        current_val: A,
        old_val: A,
        new_val: A,
    },
    Push { n: u8, current_val: A, new_val: A },
    Down {
        current_val: A,
        old_val: A,
        new_val: A,
    },
}


impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash> Instruction<TreeStack<A>>
    for TreeStackInstruction<A> {
        fn apply(&self, t: TreeStack<A>) -> Vec<TreeStack<A>> {
            match self {
                &TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } => {
                    t.up(n, current_val, Some(&old_val), new_val)
                }
                &TreeStackInstruction::Push { n, ref current_val, ref new_val } => {
                    t.up(n, current_val, None, new_val)
                }
                &TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                    t.down(current_val, old_val, new_val)
                }
            }
        }
    }


impl<A: fmt::Display> fmt::Display for TreeStackInstruction<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } => {
                write!(f, "(Up {} {} {} {})", n, current_val, old_val, new_val)
            },
            &TreeStackInstruction::Push { n, ref current_val, ref new_val } => {
                write!(f, "(Push {} {} {})", n, current_val, new_val)
            }
            &TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                write!(f, "(Down {} {} {})", current_val, old_val, new_val)
            }
        }
    }
}
