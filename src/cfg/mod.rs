use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

mod from_str;
pub mod from_pmcfg;

/// Variable or terminal symbol in an CFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum LetterT<N,T> {
    Label(N),
    Value(T),
}

/// Composition function in an CFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Composition<N,T> {
    pub composition: Vec<LetterT<N,T>>,
}

/// Rule of a weighted CFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct CFGRule<N, T, W> {
    pub head: N,
    pub composition: Composition<N,T>,
    pub weight: W,
}

/// A weighted CFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct CFG<N, T, W> {
    pub _dummy: PhantomData<T>,
    pub initial: Vec<N>,
    pub rules: Vec<CFGRule<N, T, W>>,
}

impl<N: Hash, T: Hash, W> Hash for CFGRule<N, T, W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.head.hash(state);
        self.composition.hash(state);
    }
}
