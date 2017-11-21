mod automaton;
use dyck::multiple::automaton::MultipleDyckAutomaton;
use util::partition::Partition;
pub use dyck::Bracket;
use automata::Automaton;
use std::fmt::Debug;


/// An object that represents the mutliple Dyck language of an alphabet Σ with respect to
/// a partition of Σ.
pub struct MultipleDyckLanguage<T: Ord + Clone>(
    MultipleDyckAutomaton<T>
);

impl<T: Clone + Eq + Debug + Ord> MultipleDyckLanguage<T> {
    /// Represents a multiple Dyck language with respect to
    /// a partition Π = {π₁, …, πₙ} of an implicit alphabet Σ = π₁ ∪ … ∪ πₙ.
    pub fn new(p: &Partition<T>) -> Self {
        MultipleDyckLanguage(MultipleDyckAutomaton::new(p))
    }

    /// Unweightedly recognizes an element w ∈ Σ* of a multiple Dyck language with respect to 
    /// Σ and a partition of Σ.
    pub fn recognize(&self, word: &Vec<Bracket<T>>) -> bool {
        let &MultipleDyckLanguage(ref mda) = self;
        mda.recognise(word.clone()).next().is_some()
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn mutliple_dyck_language() {
        use super::Bracket::*;
        use super::MultipleDyckLanguage;
        use util::partition::Partition;
        
        let words = vec![
            vec![Open(1), Close(1), Open(2), Close(2)],
            vec![Open(1), Open(2), Close(2), Open(1), Close(1), Close(1), Open(2), Close(2)],
            vec![Open(1), Open(2), Close(2), Close(1), Open(2), Open(1), Close(1), Close(2)],
            vec![Open(1), Open(3), Close(3), Close(1), Open(2), Open(4), Close(4), Close(2)]
        ];

        let partition = Partition::new(
            vec![ vec![1,2].into_iter().collect(),
                  vec![3,4].into_iter().collect() ]
        ).unwrap();

        let mdl = MultipleDyckLanguage::new(&partition);

        for dyckword in words {
            assert!(mdl.recognize(&dyckword));
        }

        let not_words = vec![
            vec![Open(1), Close(2), Open(2), Close(1)],
            vec![Open(1), Open(4), Close(4), Close(1), Open(3), Open(2), Close(2), Close(3)]
        ];

        for not_dyckword in not_words {
            assert!(!mdl.recognize(&not_dyckword));
        }
    }
}