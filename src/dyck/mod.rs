pub mod multiple;

use std::cmp::PartialEq;
use std::hash::Hash;
use std::fmt::Debug;

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
        match bracket {
            &Bracket::Open(ref symbol) => {
                stack.push(symbol);
            }
            &Bracket::Close(ref symbol) => match stack.pop() {
                None => return false,
                Some(symbol_) => if symbol != symbol_ {
                    return false;
                },
            },
        }
    }

    stack.is_empty()
}

/// Reckognizes a bracket word as element of the Dyck language over the set of elements in `A`.
/// Implementation using `PushDownAutomaton`.
pub fn recognize_with_pda<A>(
    alphabet: Vec<A>,
    word: Vec<Bracket<A>>,
) -> bool 
where A: Ord + Hash + Debug + Clone + PartialOrd + PartialEq + Eq
{
    use PushDownInstruction::Replace;
    use Transition;
    use PushDown;
    use PushDownAutomaton;
    use Automaton;

    let mut instructions = Vec::new();
    for alpha in &alphabet {
        instructions.push(Transition {
            word: vec![Bracket::Open(alpha.clone())],
            weight: 1 as u8,
            instruction: Replace {
                current_val: vec![None],
                new_val: vec![None, Some(alpha.clone())],
            },
        });
        instructions.push(Transition {
            word: vec![Bracket::Close(alpha.clone())],
            weight: 1 as u8,
            instruction: Replace {
                current_val: vec![Some(alpha.clone()), None],
                new_val: vec![None],
            },
        });
        for beta in &alphabet {
            instructions.push(Transition {
                word: vec![Bracket::Open(alpha.clone())],
                weight: 1 as u8,
                instruction: Replace {
                    current_val: vec![Some(beta.clone())],
                    new_val: vec![Some(beta.clone()), Some(alpha.clone())],
                },
            });
            instructions.push(Transition {
                word: vec![Bracket::Close(alpha.clone())],
                weight: 1 as u8,
                instruction: Replace {
                    current_val: vec![Some(alpha.clone()), Some(beta.clone())],
                    new_val: vec![Some(beta.clone())],
                },
            });
        }
    }

    let automaton = PushDownAutomaton::new(
        instructions,
        PushDown::from_vec(vec![None]),
    );
    let ffc = automaton.recognise(word).next();

    ffc.is_some()
}

use std::fmt::{Display, Formatter, Error};

impl<T> Display for Bracket<T>
where T: Display + PartialEq
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            &Bracket::Open(ref t) => {
                write!(f, "⟨{}", t)
            },
            &Bracket::Close(ref t) => {
                write!(f, "{}⟩", t)
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

    #[test]
    fn dyck_with_pda() {
        use super::Bracket::*;
        let words = vec![
            vec![Open(1), Close(1), Open(2), Close(2)],
            vec![Open(1), Open(2), Close(2), Open(1), Close(1), Close(1)],
        ];
        let alphabet = vec![1, 2];

        for dyckword in words.into_iter() {
            assert!(super::recognize_with_pda(alphabet.clone(), dyckword));
        }

        let alphabet = vec!["eins", "zwei"];
        assert!(super::recognize_with_pda(
            alphabet,
            vec![Open("eins"), Close("eins"), Open("zwei"), Close("zwei")]
        ));
    }
}
