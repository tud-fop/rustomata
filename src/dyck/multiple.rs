use util::partition::Partition;

pub use dyck::Bracket;

use std::hash::Hash;
use std::collections::BTreeSet;
use ::std::fmt::Debug;

/// Power set.
fn power<T: Clone + Ord>(set: &BTreeSet<T>) -> Vec<BTreeSet<T>> {
    (0..2usize.pow(set.len() as u32)).map(|i| {
         set.iter().enumerate().filter(|&(t, _)| (i >> t) % 2 == 1)
                             .map(|(_, element)| element.clone())
                             .collect()
     }).collect()
}

/// An object that represents the mutliple Dyck language of an alphabet Σ with respect to
/// a partition of Σ.
#[derive(Debug, Serialize, Deserialize)]
pub struct MultipleDyckLanguage<T: Ord + Clone> {
    partition: Partition<T>
}

impl<'a, T: Clone + Ord + Debug> MultipleDyckLanguage<T> {
    /// Represents a multiple Dyck language with respect to
    /// a partition Π = {π₁, …, πₙ} of an implicit alphabet Σ = π₁ ∪ … ∪ πₙ.
    pub fn new(cells: Vec<BTreeSet<T>>) -> Option<Self> {
        match Partition::new(cells) {
            Some(partition) => Some(MultipleDyckLanguage{ partition }),
            None => None
        }
    }
}

/// An element of the tree push-down for recognizing a MDL.
#[derive(Debug, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
enum MDTreeElem<A> {
    Root,
    Node(Option<A>, BTreeSet<A>)
}

impl<'a, T: Hash + Clone + Ord + Debug> MultipleDyckLanguage<T> {
    /// Unweightedly recognizes an element w ∈ Σ* of a multiple Dyck language with respect to 
    /// Σ and a partition of Σ.
    pub fn recognize(&self, word: Vec<Bracket<T>>) -> bool {
        use TreeStackAutomaton;
        use TreeStackInstruction;
        use Transition;
        use std::marker::PhantomData;
        use std::collections::BTreeSet; // HashSet does not implement Hash
        use Automaton;
        use TreeStack;
        use Configuration;

        let mut transitions = Vec::new();

        
        for sigma in self.partition.alphabet() {
            let mut next_subset: BTreeSet<T> = self.partition.get_cell(sigma).unwrap().iter().cloned().collect();
            next_subset.remove(sigma);
            transitions.push(Transition{
                _dummy: PhantomData,
                word: vec![Bracket::Open(sigma.clone())],
                weight: 1,
                instruction: TreeStackInstruction::NDPush{
                    lower_current: MDTreeElem::Root,
                    upper_new: MDTreeElem::Node(Some(sigma.clone()), next_subset)
                }
            });
            for subset in power(self.partition.get_cell(sigma).unwrap()).into_iter() {
                if subset.contains(sigma) {
                    let mut next_subset = subset.clone();
                    next_subset.remove(sigma);
                    transitions.push(Transition{
                        _dummy: PhantomData,
                        word: vec![Bracket::Open(sigma.clone())],
                        weight: 1,
                        instruction: TreeStackInstruction::NDUp{ 
                            lower_current: MDTreeElem::Root,                // need?
                            upper_old: MDTreeElem::Node(None, subset.clone()),
                            upper_new: MDTreeElem::Node(Some(sigma.clone()), next_subset)
                        }
                    });
                } else {
                    if subset.is_empty() {
                        transitions.push(Transition{
                            _dummy: PhantomData,
                            word: vec![Bracket::Close(sigma.clone())],
                            weight: 1,
                            instruction: TreeStackInstruction::NDPop{
                                upper_current: MDTreeElem::Node(Some(sigma.clone()), subset.clone())
                            }
                        });
                    } else {
                        transitions.push(Transition{
                            _dummy: PhantomData,
                            word: vec![Bracket::Close(sigma.clone())],
                            weight: 1,
                            instruction: TreeStackInstruction::NDDown{
                                upper_current: MDTreeElem::Node(Some(sigma.clone()), subset.clone()),
                                upper_new: MDTreeElem::Node(None, subset.clone())
                            }
                        });
                    }
                }
            }

            for predecessor_sigma in self.partition.alphabet() {
                for p_subset in power(self.partition.get_cell(predecessor_sigma).unwrap()) {
                    if !p_subset.contains(predecessor_sigma){
                        let mut next_subset: BTreeSet<T> = self.partition.get_cell(sigma).unwrap().iter().map(|e| e.clone()).collect();
                        next_subset.remove(sigma);
                        transitions.push(Transition{
                            _dummy: PhantomData,
                            word: vec![Bracket::Open(sigma.clone())],
                            weight: 1,
                            instruction: TreeStackInstruction::NDPush{
                                lower_current: MDTreeElem::Node(Some(predecessor_sigma.clone()), p_subset.clone()),
                                upper_new: MDTreeElem::Node(Some(sigma.clone()), next_subset.clone())
                            }
                        });
                    
                        for subset in power(self.partition.get_cell(sigma).unwrap()) {
                            if subset.contains(sigma) {
                                let mut next_subset = subset.iter().filter(|x| *x != sigma).cloned().collect();
                                //next_subset.remove(sigma);
                                transitions.push(Transition{
                                    _dummy: PhantomData,
                                    word: vec![Bracket::Open(sigma.clone())],
                                    weight: 1,
                                    instruction: TreeStackInstruction::NDUp{
                                        lower_current: MDTreeElem::Node(Some(predecessor_sigma.clone()), p_subset.clone()), 
                                        upper_old: MDTreeElem::Node(None, subset.clone()),
                                        upper_new: MDTreeElem::Node(Some(sigma.clone()), next_subset)
                                    }
                                });
                            }
                        }
                    }                    
                }
            }
        }

        let automaton = TreeStackAutomaton::new(transitions, TreeStack::new(MDTreeElem::Root));

        for (Configuration{ storage, .. }, _) in automaton.recognise(word) {
            if storage == TreeStack::new(MDTreeElem::Root) {
                return true;
            }
        }

        false
    }
}


#[cfg(test)]
mod tests {

    #[test]
    fn mutliple_dyck() {
        use super::Bracket::*;
        use super::MultipleDyckLanguage;
        
        let words = vec![
            vec![Open(1), Close(1), Open(2), Close(2)],
            vec![Open(1), Open(2), Close(2), Open(1), Close(1), Close(1), Open(2), Close(2)],
            vec![Open(1), Open(2), Close(2), Close(1), Open(2), Open(1), Close(1), Close(2)],
            vec![Open(1), Open(3), Close(3), Close(1), Open(2), Open(4), Close(4), Close(2)]
        ];

        let partitions = vec![ vec![1,2].into_iter().collect(),
                               vec![3,4].into_iter().collect() ];

        let mdl = MultipleDyckLanguage::new(partitions).unwrap();

        for dyckword in words.into_iter() {
            assert!(mdl.recognize(dyckword));
        }

        let not_words = vec![
            vec![Open(1), Close(2), Open(2), Close(1)],
            vec![Open(1), Open(4), Close(4), Close(1), Open(3), Open(2), Close(2), Close(3)]
        ];

        for not_dyckword in not_words.into_iter() {
            assert!(!mdl.recognize(not_dyckword));
        }
    }
}