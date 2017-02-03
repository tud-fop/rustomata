use std::marker::PhantomData;

mod from_str;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum VarT<T> {
    /// `Var(i, j)` represents the `j`th component of the `i`th successor.
    /// Indexing starts from `0`.
    Var(u8, u8),
    T(T)
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Composition<T> { pub composition: Vec<Vec<VarT<T>>> }

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct PMCFGRule<N, T, W> {
    pub head: N,
    pub tail: Vec<N>,
    pub composition: Composition<T>,
    pub weight: W
}

#[derive(Debug, PartialEq)]
pub struct PMCFG<N, T, W> {
    pub _dummy: PhantomData<T>,
    pub initial: N,
    pub rules: Vec<PMCFGRule<N, T, W>>
}

