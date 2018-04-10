use std::cmp::Ordering;
use std::ops::Mul;
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};

use integeriser::{HashIntegeriser, Integeriser};

use recognisable::{Configuration, Instruction};
use util::integerisable::{Integerisable1, Integerisable2};

/// Transition of an automaton with `weight`, reading the sequence `word`, and applying the `instruction`.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Transition<I, T, W> {
    pub word: Vec<T>,
    pub weight: W,
    pub instruction: I,
}

impl<I, IInt, T, W> Integerisable2 for Transition<I, T, W>
where
    I: Integerisable1<AInt = IInt>,
    T: Clone + Eq + Hash,
    W: Clone,
{
    type AInt = Transition<IInt, usize, W>;
    type I1 = HashIntegeriser<T>;
    type I2 = I::I;

    fn integerise(&self, integeriser1: &mut Self::I1, integeriser2: &mut Self::I2) -> Self::AInt {
        Transition {
            word: self.word
                .iter()
                .map(|t| integeriser1.integerise(t.clone()))
                .collect(),
            weight: self.weight.clone(),
            instruction: self.instruction.integerise(integeriser2),
        }
    }

    fn un_integerise(v: &Self::AInt, integeriser1: &Self::I1, integeriser2: &Self::I2) -> Self {
        Transition {
            word: v.word
                .iter()
                .map(|i| integeriser1.find_value(*i).unwrap().clone())
                .collect(),
            weight: v.weight.clone(),
            instruction: I::un_integerise(&v.instruction, integeriser2),
        }
    }
}

impl<I, T, W> Hash for Transition<I, T, W>
where
    I: Hash,
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.word.hash(state);
        self.instruction.hash(state);
    }
}

/// `impl` of `PartialEq` that ignores the `weight` (to conform to the `impl` of `Hash`)
impl<I, T, W> PartialEq for Transition<I, T, W>
where
    I: PartialEq,
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.word == other.word && self.instruction == other.instruction
    }
}

impl<I, T, W> Eq for Transition<I, T, W>
where
    I: Eq,
    T: Eq,
{
}

impl<I, T, W> Transition<I, T, W>
where
    I: Instruction,
    I::Storage: Clone,
    T: Clone + PartialEq,
    W: Mul<Output = W> + Copy,
{
    pub fn apply(
        &self,
        c: &Configuration<I::Storage, T, W>,
    ) -> Vec<Configuration<I::Storage, T, W>> {
        if !c.word.starts_with(&self.word[..]) {
            return Vec::new();
        }

        let mut confs = Vec::new();
        for s1 in self.instruction.apply(c.storage.clone()) {
            confs.push(Configuration {
                word: c.word.clone().split_off(self.word.len()),
                storage: s1,
                weight: c.weight * self.weight,
            })
        }

        confs
    }
}

impl<I, T, W> PartialOrd for Transition<I, T, W>
where
    I: Eq + PartialOrd,
    T: Eq + PartialOrd,
    W: Eq + PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.weight.partial_cmp(&other.weight) {
            None |
            Some(Ordering::Equal) => {
                match self.word.partial_cmp(&other.word) {
                    None |
                    Some(Ordering::Equal) => self.instruction.partial_cmp(&other.instruction),
                    x => x,
                }
            }
            x => x,
        }
    }
}

impl<I, T, W> Ord for Transition<I, T, W>
where
    I: Eq + Ord,
    T: Eq + Ord,
    W: Eq + Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        match self.weight.cmp(&other.weight) {
            Ordering::Equal => {
                match self.word.cmp(&other.word) {
                    Ordering::Equal => self.instruction.cmp(&other.instruction),
                    x => x,
                }
            }
            x => x,
        }
    }
}

impl<I, T, W> Display for Transition<I, T, W>
where
    I: Display,
    T: Debug,
    W: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Transition {:?} {}  # {}",
            self.word,
            self.instruction,
            self.weight
        )
    }
}
