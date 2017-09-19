use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ops::Mul;
use std::fmt;
use std::hash::{Hash, Hasher};

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

impl<A, I: Hash + Instruction<A>, T: Hash, W> Hash for Transition<A, I, T, W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.word.hash(state);
        self.instruction.hash(state);
    }
}

impl<A: Clone, I: Instruction<A>, T: PartialEq + Clone, W: Mul<Output = W> + Copy> Transition<A, I, T, W> {
    pub fn apply(&self, c: &Configuration<A, T, W>) -> Vec<Configuration<A, T, W>> {
        if !c.word.starts_with(&self.word[..]) {
            return Vec::new()
        }

        let mut confs = Vec::new();
        for s1 in self.instruction.apply(c.storage.clone()) {
            confs.push(
                Configuration {
                    word: c.word.clone().split_off(self.word.len()),
                    storage: s1,
                    weight: c.weight * self.weight,
                }
            )
        }

        confs
    }
}

impl<A: Eq, I: Instruction<A> + Eq, T: Eq, W: PartialOrd + Eq> PartialOrd for Transition<A, I, T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.weight == other.weight{
            self.word.len().partial_cmp(&other.word.len())
        }
        else{
            self.weight.partial_cmp(&other.weight)
        }
    }
}

impl<A: Eq, I: Instruction<A> + Eq, T: Eq, W: Ord + Eq> Ord for Transition<A, I, T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.weight == other.weight{
            self.word.len().cmp(&other.word.len())
        }
        else{
            self.weight.cmp(&other.weight)
        }
    }
}

impl<A, I: Instruction<A> + fmt::Display, T: fmt::Debug, W: fmt::Display> fmt::Display for Transition<A, I, T, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Transition {:?} {}  # {}", self.word, self.instruction, self.weight)
    }
}
