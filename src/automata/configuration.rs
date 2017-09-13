use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::fmt;

/// Configuration of an automaton containing sequence of symbols `word` to be read, a storage value `storage`, and a `weight`.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Configuration<S, T, W> {
    pub word: Vec<T>,
    pub storage: S,
    pub weight: W,
}

impl<S: Hash, T: Hash, W> Hash for Configuration<S, T, W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.word.hash(state);
        self.storage.hash(state);
    }
}

impl<S: Eq, T: Eq, W: PartialOrd + Eq> PartialOrd for Configuration<S, T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.weight == other.weight{
            other.word.len().partial_cmp(&self.word.len())
        }
        else{
            self.weight.partial_cmp(&other.weight)
        }
    }
}

impl<S: Eq, T: Eq, W: Ord + Eq> Ord for Configuration<S, T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.weight == other.weight{
            other.word.len().cmp(&self.word.len())
        }
        else{
            self.weight.cmp(&other.weight)
        }
    }
}

impl<S: fmt::Display,
     T: fmt::Display,
     W: fmt::Display
    >fmt::Display for Configuration<S, T, W> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let mut buffer = String::new();
            let mut iter1 = self.word.iter().peekable();

            while let Some(nt) = iter1.next() {
                buffer.push_str(format!("{}", nt).as_str());
                if iter1.peek().is_some() {
                    buffer.push_str(" ");
                }
            }
            write!(f, "Configuration:\nword:[{}]\nweight:{}\n{}\n", buffer, self.weight, self.storage)
        }
    }
