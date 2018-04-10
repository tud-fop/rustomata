mod automaton;
use dyck::multiple::automaton::MultipleDyckAutomaton;
use util::partition::Partition;
pub use dyck::Bracket;
use recognisable::automaton::recognise;

/// An object that represents the mutliple Dyck language of an alphabet Σ with respect to
/// a partition of Σ.
#[derive(Debug, Serialize, Deserialize)]
pub struct MultipleDyckLanguage<T: Ord + Clone>(MultipleDyckAutomaton<T>);

impl<'a, T: Clone + Eq + Ord> MultipleDyckLanguage<T> {
    /// Represents a multiple Dyck language with respect to
    /// a partition `p` = {p_1, ..., p_k} of an implicit alphabet p1 + ... + pk.
    pub fn new(p: Partition<T>) -> Self {
        MultipleDyckLanguage(MultipleDyckAutomaton::new(p))
    }

    /// Represents a sorted multiple Dyck language with respect to a
    /// partition `p` = {p_1, ..., p_k} of an implicit sorted alphabet (p1 + ... + pk, `sort`).
    pub fn sorted<F>(p: Partition<T>, sort: F) -> Self
    where
        F: Fn(&T) -> usize,
    {
        MultipleDyckLanguage(MultipleDyckAutomaton::sorted(p, sort))
    }

    /// Returns true iff `word` is a member of the multiple Dyck language.
    pub fn recognize(&self, word: &[Bracket<T>]) -> bool {
        let &MultipleDyckLanguage(ref mda) = self;
        let word_ = word.to_owned();
        let mut b = recognise(mda, word_);
        b.next().is_some()
    }
}

#[cfg(test)]
mod test {
    use super::Bracket::*;
    use super::MultipleDyckLanguage;
    use util::partition::Partition;

    #[test]
    fn mutliple_dyck_language() {
        let words = vec![
            vec![Open(1), Close(1), Open(2), Close(2)],
            vec![
                Open(1),
                Open(2),
                Close(2),
                Open(1),
                Close(1),
                Close(1),
                Open(2),
                Close(2),
            ],
            vec![
                Open(1),
                Open(2),
                Close(2),
                Close(1),
                Open(2),
                Open(1),
                Close(1),
                Close(2),
            ],
            vec![
                Open(1),
                Open(3),
                Close(3),
                Close(1),
                Open(2),
                Open(4),
                Close(4),
                Close(2),
            ],
        ];

        let partition = Partition::new(vec![
            vec![1, 2].into_iter().collect(),
            vec![3, 4].into_iter().collect(),
        ]).unwrap();

        let mdl = MultipleDyckLanguage::new(partition);

        for dyckword in words {
            assert!(mdl.recognize(&dyckword));
        }

        let not_words = vec![
            vec![Open(1), Close(2), Open(2), Close(1)],
            vec![
                Open(1),
                Open(4),
                Close(4),
                Close(1),
                Open(3),
                Open(2),
                Close(2),
                Close(3),
            ],
        ];

        eprintln!("{:?}", mdl.recognize(&not_words[0]));
        eprintln!("{:?}", mdl.recognize(&not_words[1]));
        // assert!(!mdl.recognize(&not_words[0]));
        // assert!(!mdl.recognize(&not_words[1]));
        // for not_dyckword in not_words {
        //     assert!(!mdl.recognize(&not_dyckword));
        // }
    }

    #[test]
    fn sorted_multiple_dyck_language() {
        let partition = Partition::new(vec![
            vec![1, 2].into_iter().collect(),
            vec![3, 4].into_iter().collect(),
        ]).unwrap();

        let sort = |i: &usize| -> usize {
            if vec![1usize, 2usize].contains(i) {
                1
            } else {
                2
            }
        };

        let l = MultipleDyckLanguage::sorted(partition, sort);

        let words = vec![
            vec![Open(1), Close(1), Open(2), Close(2)],
            vec![
                Open(1),
                Open(2),
                Close(2),
                Open(1),
                Close(1),
                Close(1),
                Open(2),
                Close(2),
            ],
            vec![
                Open(1),
                Open(2),
                Close(2),
                Close(1),
                Open(2),
                Open(1),
                Close(1),
                Close(2),
            ],
            vec![
                Open(1),
                Open(3),
                Close(3),
                Close(1),
                Open(2),
                Open(4),
                Close(4),
                Close(2),
            ],
        ];
        for sdw in words {
            assert!(l.recognize(&sdw));
        }

        let unwords = vec![
            vec![Open(1), Close(2), Open(2), Close(1)],
            vec![
                Open(1),
                Close(1),
                Close(2),
                Open(2),
                Open(1),
                Close(1),
                Close(2),
                Open(2),
            ],
            vec![
                Open(1),
                Open(4),
                Close(4),
                Close(1),
                Open(3),
                Open(2),
                Close(2),
                Close(3),
            ],
        ];

        for unword in unwords {
            assert!(!l.recognize(&unword));
        }
    }
}
