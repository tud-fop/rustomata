use std::fmt;
use std::hash::Hash;

use automata::Instruction;
use tree_stack_automaton::TreeStack;


/// Instruction on `TreeStack<A>`s.
#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum TreeStackInstruction<A> {
    Up {
        n: usize,
        current_val: A,
        old_val: A,
        new_val: A,
    },
    Push { n: usize, current_val: A, new_val: A },
    Down {
        current_val: A,
        old_val: A,
        new_val: A,
    }
}


impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash> Instruction<TreeStack<A>>
    for TreeStackInstruction<A> {
        fn apply(&self, t: TreeStack<A>) -> Vec<TreeStack<A>> {
            match *self {
                TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } => {
                    if t.current_symbol() == current_val {
                        match t.clone().up(n) {
                            Ok(child) => {
                                if child.current_symbol() == old_val {
                                    vec![child.set(new_val.clone())]
                                } else {
                                    vec![]
                                }
                            },
                            Err(_) => {
                                vec![]
                            },
                        }
                    } else {
                        vec![]
                    }
                }
                TreeStackInstruction::Push { n, ref current_val, ref new_val } => {
                    if t.current_symbol() == current_val {
                        t.clone().push(n, new_val.clone()).into_iter().collect()
                    } else {
                        vec![]
                    }
                }
                TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                    if t.current_symbol() == current_val {
                        match t.clone().down() {
                            Ok(parent) => {
                                if parent.current_symbol() == old_val {
                                    vec![parent.set(new_val.clone())]
                                } else {
                                    vec![]
                                }
                            },
                            _ => vec![],
                        }
                    } else {
                        vec![]
                    }
                }
            }
        }
    }


impl<A: fmt::Display> fmt::Display for TreeStackInstruction<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } =>
                write!(f, "(Up {} {} {} {})", n, current_val, old_val, new_val),
            TreeStackInstruction::Push { n, ref current_val, ref new_val } =>
                write!(f, "(Push {} {} {})", n, current_val, new_val),
            TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } =>
                write!(f, "(Down {} {} {})", current_val, old_val, new_val),
        }
    }
}
