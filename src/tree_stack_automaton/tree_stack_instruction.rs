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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tree_stack_instruction_map_correctness() {
        let inputs = vec![
            (TreeStackInstruction::Up { n: 1, current_val: 1, old_val: 2, new_val: 3 },
                TreeStackInstruction::Up { n: 1, current_val: 2, old_val: 4, new_val: 6 }),
            (TreeStackInstruction::Down { current_val: 1, old_val: 2, new_val: 3 },
                TreeStackInstruction::Down { current_val: 2, old_val: 4, new_val: 6 }),
            (TreeStackInstruction::Push { n: 1, current_val: 1, new_val: 2 },
                TreeStackInstruction::Push { n: 1, current_val: 2, new_val: 4 }),
        ];

        for (instruction, mapped_control_instruction) in inputs {
            assert_eq!(
                mapped_control_instruction,
                instruction.map(&|x: &u8| x * 2)
            );
        }
    }

    #[test]
    fn test_tree_stack_instruction_map_inverse() {
        let instruction = TreeStackInstruction::Up { n: 1, current_val: 1, old_val: 2, new_val: 3 };
        let mapped_instruction = instruction.map(&|x: &u8| x * 2);

        assert_eq!(
            instruction,
            mapped_instruction.map(&|x: &u8| x / 2)
        );
    }

    #[test]
    fn test_tree_stack_instruction_apply_correctness() {
        let mut tree_stack = TreeStack::new('@').push(1, 'a').unwrap().down().unwrap();

        let up_instruction = TreeStackInstruction::Up {
            n: 1, current_val: '@', old_val: 'a', new_val: 'a'
        };
        assert_eq!(
            vec![tree_stack.clone().up(1).unwrap()],
            up_instruction.apply(tree_stack.clone())
        );

        tree_stack = tree_stack.up(1).unwrap();
        let down_instruction = TreeStackInstruction::Down {
            current_val: 'a', old_val: '@', new_val: '@'
        };
        assert_eq!(
            vec![tree_stack.clone().down().unwrap()],
            down_instruction.apply(tree_stack.clone())
        );

        tree_stack = tree_stack.down().unwrap();
        let push_instruction = TreeStackInstruction::Push {
            n: 2, current_val: '@', new_val: 'b'
        };
        assert_eq!(
            vec![tree_stack.clone().push(2, 'b').unwrap()],
            push_instruction.apply(tree_stack.clone())
        );

        let invalid_instructions = vec![
            TreeStackInstruction::Up { n: 1, current_val: 'x', old_val: 'a', new_val: 'a' },
            TreeStackInstruction::Down { current_val: '@', old_val: 'x', new_val: 'x' },
            TreeStackInstruction::Push { n: 1, current_val: '@', new_val: 'y' },
        ];

        for invalid_instruction in invalid_instructions {
            assert_eq!(
                Vec::<TreeStack<char>>::new(),
                invalid_instruction.apply(tree_stack.clone())
            );
        }
    }

    #[test]
    fn test_tree_stack_instruction_apply_inverse() {
        let tree_stack = TreeStack::new('@').push(1, 'a').unwrap().down().unwrap();
        let up_instruction = TreeStackInstruction::Up {
            n: 1, current_val: '@', old_val: 'a', new_val: 'a'
        };
        let down_instruction = TreeStackInstruction::Down {
            current_val: 'a', old_val: '@', new_val: '@'
        };
        let upped_tree_stack = up_instruction.apply(tree_stack.clone()).pop().unwrap();

        assert_eq!(
            vec![tree_stack],
            down_instruction.apply(upped_tree_stack)
        );
    }

    #[test]
    fn test_tree_stack_instruction_integerise_inverse() {
        let instructions = vec![
            TreeStackInstruction::Up { n: 1, current_val: 1, old_val: 2, new_val: 3 },
            TreeStackInstruction::Down { current_val: 1, old_val: 2, new_val: 3 },
            TreeStackInstruction::Push { n: 1, current_val: 1, new_val: 2 },
        ];
        let mut integeriser = HashIntegeriser::new();

        for instruction in instructions {
            let integerised_instruction = instruction.integerise(&mut integeriser);

            assert_eq!(
                instruction,
                TreeStackInstruction::un_integerise(&integerised_instruction, &integeriser)
            );
        }
    }
}
