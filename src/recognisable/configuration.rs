use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::fmt;

/// Configuration of an automaton containing sequence of symbols `word` to be read, a storage value `storage`, and a `weight`.
#[derive(Clone, Debug)]
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

/// `impl` of `PartialEq` that ignores `weight` (to conform to the `impl` of `Hash`)
impl<S: PartialEq, T: PartialEq, W> PartialEq for Configuration<S, T, W> {
    fn eq(&self, other: &Self) -> bool {
        self.word == other.word && self.storage == other.storage
    }
}

impl<S: Eq, T: Eq, W> Eq for Configuration<S, T, W> {}

impl<S, T, W> PartialOrd for Configuration<S, T, W>
where
    S: PartialOrd + PartialEq,
    T: PartialOrd + PartialEq,
    W: PartialOrd + PartialEq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.weight.partial_cmp(&other.weight) {
            None |
            Some(Ordering::Equal) => {
                match other.word.len().partial_cmp(&self.word.len()) {
                    None |
                    Some(Ordering::Equal) => {
                        match self.word.partial_cmp(&other.word) {
                            None |
                            Some(Ordering::Equal) => self.storage.partial_cmp(&other.storage),
                            x => x,
                        }
                    }
                    x => x,
                }
            }
            x => x,
        }
    }
}

impl<S, T, W> Ord for Configuration<S, T, W>
where
    S: Eq + Ord,
    T: Eq + Ord,
    W: Eq + Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        match self.weight.cmp(&other.weight) {
            Ordering::Equal => {
                match other.word.len().cmp(&self.word.len()) {
                    Ordering::Equal => {
                        match self.word.cmp(&other.word) {
                            Ordering::Equal => self.storage.cmp(&other.storage),
                            x => x,
                        }
                    }
                    x => x,
                }
            }
            x => x,
        }
    }
}

impl<S: fmt::Display, T: fmt::Display, W: fmt::Display> fmt::Display for Configuration<S, T, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        let mut iter1 = self.word.iter().peekable();

        while let Some(nt) = iter1.next() {
            buffer.push_str(format!("{}", nt).as_str());
            if iter1.peek().is_some() {
                buffer.push_str(" ");
            }
        }
        write!(
            f,
            "Configuration:\nword:[{}]\nweight:{}\n{}\n",
            buffer,
            self.weight,
            self.storage
        )
    }
}
