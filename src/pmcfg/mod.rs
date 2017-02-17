use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

mod from_str;

/// Variable or terminal symbol in an MCFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum VarT<T> {
    /// `Var(i, j)` represents the `j`th component of the `i`th successor.
    /// Indexing starts from `0`.
    Var(u8, u8),
    T(T),
}

/// Composition function in an MCFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Composition<T> {
    pub composition: Vec<Vec<VarT<T>>>,
}

/// Rule of a weighted MCFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct PMCFGRule<N, T, W> {
    pub head: N,
    pub tail: Vec<N>,
    pub composition: Composition<T>,
    pub weight: W,
}

/// A weighted MCFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct PMCFG<N, T, W> {
    pub _dummy: PhantomData<T>,
    pub initial: N,
    pub rules: Vec<PMCFGRule<N, T, W>>,
}

impl<N: Hash, T: Hash, W> Hash for PMCFGRule<N, T, W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.head.hash(state);
        self.tail.hash(state);
        self.composition.hash(state);
    }
}
