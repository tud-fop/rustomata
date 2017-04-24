use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ops::Mul;
use std::fmt;

use automata::Instruction;
use automata::Configuration;

/// Transition of an automaton with `weight`, reading the sequence `word`, and applying the `instruction`.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Transition<A, I: Instruction<A>, T, W> {
    pub _dummy: PhantomData<A>,
    pub word: Vec<T>,
    pub weight: W,
    pub instruction: I,
}


impl<A: Clone, I: Instruction<A>, T: PartialEq + Clone, W: Mul<Output = W> + Copy> Transition<A, I, T, W> {
    pub fn apply(&self, c: &Configuration<A, T, W>) -> Option<Configuration<A, T, W>> {
        if !c.word.starts_with(&self.word[..]) {
            return None;
        }

        match self.instruction.apply(c.storage.clone()) {
            Some(t1) => {
                Some(Configuration {
                    word: c.word.clone().split_off(self.word.len()),
                    storage: t1,
                    weight: c.weight * self.weight,
                })
            }
            _ => None,
        }
    }
}

impl<A: Eq, I: Instruction<A> + Eq, T: Eq, W: PartialOrd + Eq> PartialOrd for Transition<A, I, T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.weight.partial_cmp(&other.weight)
    }
}

impl<A: Eq, I: Instruction<A> + Eq, T: Eq, W: Ord + Eq> Ord for Transition<A, I, T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.weight.cmp(&other.weight)
    }
}

impl<A, I: Instruction<A> + fmt::Display, T: fmt::Debug, W: fmt::Display> fmt::Display for Transition<A, I, T, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Transition {:?} {}  # {}", self.word, self.instruction, self.weight)
    }
}
