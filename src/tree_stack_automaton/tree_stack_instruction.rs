use std::fmt;
use std::hash::Hash;

use recognisable::Instruction;
use util::integerisable::Integerisable1;
use tree_stack_automaton::TreeStack;

use integeriser::{HashIntegeriser, Integeriser};


/// Instruction on `TreeStack<A>`s.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
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
}


impl<A> TreeStackInstruction<A> {
    fn map<F, B>(&self, mut f: F) -> TreeStackInstruction<B>
        where F: FnMut(&A) -> B
    {
        match *self {
            TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } =>
                TreeStackInstruction::Up { n,
                                           current_val: f(current_val),
                                           old_val: f(old_val),
                                           new_val: f(new_val),
                },
            TreeStackInstruction::Push { n, ref current_val, ref new_val } =>
                TreeStackInstruction::Push { n,
                                             current_val: f(current_val),
                                             new_val: f(new_val),
                },
            TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } =>
                TreeStackInstruction::Down { current_val: f(current_val),
                                             old_val: f(old_val),
                                             new_val: f(new_val),
                },
        }
    }
}


impl<A: Ord + PartialEq + Clone + Hash> Instruction for TreeStackInstruction<A> {
    type Storage = TreeStack<A>;

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


impl<A: Clone + Eq + Hash> Integerisable1 for TreeStackInstruction<A> {
    type AInt = TreeStackInstruction<usize>;
    type I = HashIntegeriser<A>;

    fn integerise(&self, integeriser: &mut Self::I) -> Self::AInt {
        self.map(|a| integeriser.integerise(a.clone()))
    }

    fn un_integerise(v: &Self::AInt, integeriser: &Self::I) -> Self {
        v.map(|i| integeriser.find_value(*i).unwrap().clone())
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
