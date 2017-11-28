use automata::*;
use std::marker::PhantomData;
use std::hash::{Hash, Hasher};

/// Structure used to map weight changes. Contains only an `Instruction` and a `Vec<T>`
#[derive(Debug, Clone, PartialOrd, Ord)]
pub struct TransitionKey<I, T, W>{
    pub _dummy_w : PhantomData<W>,
    pub instruction : I,
    pub word: Vec<T>,
}

impl <I: Clone + Hash + Instruction, T: Clone + Hash , W> Hash for TransitionKey<I, T, W>{
    fn hash<H: Hasher>(&self, state: &mut H){
        self.instruction.hash(state);
        self.word.hash(state);
    }
}

/// `impl` of `PartialEq` that ignores the `weight` (to conform to the `impl` of `Hash`)
impl<I: PartialEq, T: PartialEq, W> PartialEq for TransitionKey<I, T, W> {
    fn eq(&self, other: &Self) -> bool {
        self.word == other.word && self.instruction == other.instruction
    }
}

impl<I: Eq, T: Eq, W> Eq for TransitionKey<I, T, W> {}

/// Trait defining the ability to reduce redundancy in the structure. Until now only used by `PushDownAutomaton`
pub trait Redundancy{
    fn reduce_redundancy(self) -> Self;
}

impl<I: Clone + Instruction, T: Clone , W> TransitionKey<I, T, W>{
    pub fn new(t : &Transition<I, T, W>) -> TransitionKey<I, T, W>{
        TransitionKey{
            instruction: t.instruction.clone(),
            word: t.word.clone(),
            _dummy_w: PhantomData,
        }
    }
}
