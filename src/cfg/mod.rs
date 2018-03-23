use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

mod from_str;
mod from_pmcfg;

/// Variable or terminal symbol in an `CFG`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum LetterT<N, T> {
    Label(N),
    Value(T),
}

/// Composition function in an `CFG`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct CFGComposition<N, T> {
    pub composition: Vec<LetterT<N, T>>,
}

impl<N, T> From<Vec<LetterT<N, T>>> for CFGComposition<N, T> {
    fn from(encapsulated_value: Vec<LetterT<N, T>>) -> Self {
        CFGComposition { composition: encapsulated_value }
    }
}

/// Rule of a weighted `CFG`.
#[derive(Debug, PartialOrd, Ord, Clone)]
pub struct CFGRule<N, T, W> {
    pub head: N,
    pub composition: CFGComposition<N,T>,
    pub weight: W,
}

/// A weighted `CFG`. Contains initial symbols and a set of `CFGRule`
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

impl<N: PartialEq, T: PartialEq, W> PartialEq for CFGRule<N, T, W> {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.composition == other.composition
    }
}

impl<N: Eq, T: Eq, W> Eq for CFGRule<N, T, W> {}

impl<N: fmt::Display, T: fmt::Display> fmt::Display for CFGComposition<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = "".to_string();

        let mut iter = self.composition.iter().peekable();

        buffer.push_str("[");
        while let Some(proj) = iter.next() {
            buffer.push_str(format!("{}", proj).as_str());
                if iter.peek().is_some() {
                buffer.push_str(", ");
            }
        }
        buffer.push_str("]");

        write!(f, "{}", buffer)
    }
}

impl<N: fmt::Display, T: fmt::Display> fmt::Display for LetterT<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LetterT::Value(ref x) =>
                write!(f, "T \"{}\"", x),
            LetterT::Label(ref x) =>
                write!(f, "Nt \"{}\"", x),
        }
    }
}

impl<N: fmt::Display, T: fmt::Display, W: fmt::Display> fmt::Display for CFGRule<N, T, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\" → {}  # {}", self.head, self.composition, self.weight)
    }
}
