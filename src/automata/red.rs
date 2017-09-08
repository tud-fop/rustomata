use automata::*;
use std::marker::PhantomData;
use std::hash::{Hash, Hasher};

/// Structure used to map weight changes. Contains only an `Instruction` and a `Vec<T>`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TransitionKey<A, I, T, W>{
    pub _dummy_a : PhantomData<A>,
    pub _dummy_w : PhantomData<W>,
    pub instruction : I,
    pub word: Vec<T>,
}

impl <A, I: Clone + Hash + Instruction<A>, T: Clone + Hash , W> Hash for TransitionKey<A, I, T, W>{
    fn hash<H: Hasher>(&self, state: &mut H){
        self.instruction.hash(state);
        self.word.hash(state);
    }
}

/// Trait defining the ability to reduce redundancy in the structure. Until now only used by `PushDownAutomaton`
pub trait Redundancy{
    fn reduce_redundancy(self) -> Self;
}

impl <A, I: Clone + Instruction<A>, T: Clone , W> TransitionKey<A, I, T, W>{
    pub fn new(t : &Transition<A, I, T, W>) -> TransitionKey<A, I, T, W>{
        TransitionKey{
            instruction: t.instruction.clone(),
            word: t.word.clone(),
            _dummy_a: PhantomData,
            _dummy_w: PhantomData,
        }
    }
}
