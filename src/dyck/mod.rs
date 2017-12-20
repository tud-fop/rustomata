use std::cmp::PartialEq;

/// A bracket with some annotation of type `A`.
#[derive(PartialEq, PartialOrd, Debug, Eq, Clone, Hash, Serialize, Deserialize, Ord)]
pub enum Bracket<A: PartialEq> {
    Open(A),
    Close(A),
}

/// Reckognizes a bracket word as element of the Dyck language over the set of elements in `A`.
/// Minimal implementation using `Vec`.
pub fn recognize<A: PartialEq>(word: &[Bracket<A>]) -> bool {
    let mut stack: Vec<&A> = Vec::new();

    for bracket in word {
        match *bracket {
            Bracket::Open(ref symbol) => {
                stack.push(symbol);
            }
            Bracket::Close(ref symbol) => match stack.pop() {
                None => return false,
                Some(symbol_) => if symbol != symbol_ {
                    return false;
                },
            },
        }
    }

    stack.is_empty()
}

use std::fmt::{Display, Formatter, Error};

impl<T> Display for Bracket<T>
where T: Display + PartialEq
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Bracket::Open(ref t) => {
                write!(f, "⟨{}", t)
            },
            Bracket::Close(ref t) => {
                write!(f, "⟩{}", t)
            }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn dyck() {
        use super::Bracket::*;
        let words = vec![
            vec![Open(1), Close(1), Open(2), Close(2)],
            vec![Open(1), Open(2), Close(2), Open(1), Close(1), Close(1)],
        ];

        for dyckword in words.into_iter() {
            assert!(super::recognize(&dyckword));
        }

        assert!(super::recognize(&vec![
            Open("eins"),
            Close("eins"),
            Open("zwei"),
            Close("zwei"),
        ]));
    }
}
