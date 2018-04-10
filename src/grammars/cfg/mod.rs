use std::fmt;
use std::hash::{Hash, Hasher};

mod from_str;
mod from_pmcfg;

/// Variable or terminal symbol in a CFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum LetterT<N, T> {
    Label(N),
    Value(T),
}

/// The composition function in a CFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct CFGComposition<N, T> {
    pub composition: Vec<LetterT<N, T>>,
}

impl<N, T> From<Vec<LetterT<N, T>>> for CFGComposition<N, T> {
    fn from(encapsulated_value: Vec<LetterT<N, T>>) -> Self {
        CFGComposition { composition: encapsulated_value }
    }
}

/// A rule of a weighted CFG.
///
/// ```
/// use std::str::FromStr;
/// use rustomata::grammars::cfg::{CFGComposition, CFGRule, LetterT};
///
/// let head = 'S';
/// let composition = CFGComposition::from(vec![
///     LetterT::Value('a'), LetterT::Label('S'), LetterT::Value('b'),
/// ]);
/// let weight = 0.4;
///
/// assert_eq!(
///     CFGRule { head, composition, weight },
///     CFGRule::from_str("S → [T a, Nt S, T b] # 0.4").unwrap()
/// );
/// ```
#[derive(Debug, PartialOrd, Ord, Clone)]
pub struct CFGRule<N, T, W> {
    pub head: N,
    pub composition: CFGComposition<N, T>,
    pub weight: W,
}

/// A weighted context-free grammar (CFG). Contains a set of initial nonterminal symbols
/// and a set of context-free rules.
///
/// ```
/// use std::str::FromStr;
/// use rustomata::grammars::cfg::{CFG, CFGRule};
///
/// let initial = vec!['S'];
/// let rules = vec![
///     CFGRule::from_str("S → [T a, Nt S, T b] # 0.4").unwrap(),
///     CFGRule::from_str("S → [] # 0.6").unwrap(),
/// ];
///
/// assert_eq!(
///     CFG::<char, char, f64> { initial, rules },
///     CFG::from_str("initial: [S]\n\
///                    S → [T a, Nt S, T b] # 0.4\n\
///                    S → []               # 0.6").unwrap()
/// );
/// ```
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct CFG<N, T, W> {
    // TODO: use HashSet
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
            LetterT::Value(ref x) => write!(f, "T \"{}\"", x),
            LetterT::Label(ref x) => write!(f, "Nt \"{}\"", x),
        }
    }
}

impl<N: fmt::Display, T: fmt::Display, W: fmt::Display> fmt::Display for CFGRule<N, T, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\"{}\" → {}  # {}",
            self.head,
            self.composition,
            self.weight
        )
    }
}
