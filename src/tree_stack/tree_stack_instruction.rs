use std::fmt;
use std::hash::Hash;

use automata::Instruction;
use tree_stack::TreeStack;


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
    },
    NDUp {
        lower_current: A,
        upper_old: A,
        upper_new: A
    },
    NDPop {
        upper_current: A
    },
    NDPush {
        lower_current: A,
        upper_new: A
    },
    NDDown {
        upper_current: A,
        upper_new: A
    }
}


impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash> Instruction<TreeStack<A>>
    for TreeStackInstruction<A> {
        fn apply(&self, t: TreeStack<A>) -> Vec<TreeStack<A>> {
            match self {
                &TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } => {
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
                &TreeStackInstruction::Push { n, ref current_val, ref new_val } => {
                    if t.current_symbol() == current_val {
                        t.clone().push(n, new_val.clone()).into_iter().collect()
                    } else {
                        vec![]
                    }
                }
                &TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
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
                &TreeStackInstruction::NDUp { ref lower_current, ref upper_old, ref upper_new } => {
                    if t.current_symbol() == lower_current {
                        t.ups().into_iter()
                               .filter(| a | a.current_symbol() == upper_old)
                               .map(| a | a.set(upper_new.clone()))
                               .collect()
                    } else {
                        vec![]
                    }
                }
                &TreeStackInstruction::NDPop { ref upper_current } => {
                    if t.current_symbol() == upper_current {
                        match t.pop() {
                            Ok(t_) => vec![t_],
                            _ => vec![]
                        }
                    } else {
                        vec![]
                    }
                }
                &TreeStackInstruction::NDPush { ref lower_current, ref upper_new } => {
                    if t.current_symbol() == lower_current {
                        vec![t.push_next(upper_new.clone())]
                    } else {
                        vec![]
                    }
                }
                &TreeStackInstruction::NDDown { ref upper_current, ref upper_new } => {
                    if t.current_symbol() == upper_current {
                        match t.set(upper_new.clone()).down() {
                            Ok(t) => vec![t],
                            _ => vec![]
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
        match self {
            &TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } => {
                write!(f, "(Up {} {} {} {})", n, current_val, old_val, new_val)
            },
            &TreeStackInstruction::Push { n, ref current_val, ref new_val } => {
                write!(f, "(Push {} {} {})", n, current_val, new_val)
            },
            &TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                write!(f, "(Down {} {} {})", current_val, old_val, new_val)
            },
            &TreeStackInstruction::NDUp { ref lower_current, ref upper_old, ref upper_new } => {
                write!(f, "(Up (nd) {} {} {})", lower_current, upper_old, upper_new)
            }
            &TreeStackInstruction::NDPush { ref lower_current, ref upper_new } => {
                write!(f, "(Push (nd) {} {})", lower_current, upper_new)
            }
            &TreeStackInstruction::NDPop { ref upper_current } => {
                write!(f, "(Pop {})", upper_current, )
            }
            &TreeStackInstruction::NDDown { ref upper_current, ref upper_new } => {
                write!(f, "(Down (nd) {} {})", upper_current, upper_new)
            }
        }
    }
}
