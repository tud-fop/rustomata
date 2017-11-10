pub use dyck::Bracket;

use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::collections::BTreeSet;
use ::std::fmt::Debug;

fn power<T: Clone + Ord>(set: &Vec<T>) -> Vec<BTreeSet<T>> {
    (0..2usize.pow(set.len() as u32)).map(|i| {
         set.iter().enumerate().filter(|&(t, _)| (i >> t) % 2 == 1)
                             .map(|(_, element)| element.clone())
                             .collect()
     }).collect()
}

#[derive(Debug)]
pub struct MultipleDyckLanguage<T: Eq + Hash> {
    alphabet: Vec<T>,
    partition: HashMap<T, Vec<T>>
}

impl<T: Eq + Hash + Clone + Debug> MultipleDyckLanguage<T> {
    pub fn new(alphabet: Vec<T>, cells: Vec<Vec<T>>) -> Self {
        let mut partition = HashMap::new();

        for cell in cells {
            for symbol in cell.clone() {
                if !partition.insert(symbol, cell.clone()).is_none(){
                    panic!("Partition is inconsistent.");
                }
            }
        }

        {
            // check partition and alphabet
            let alphs: HashSet<&T> = partition.keys().collect();
            let alphs_: HashSet<&T> = alphabet.iter().collect();
            if alphs != alphs_ {
                panic!("Partition and/or alphabet are inconsistent.\n {:?} \n vs \n {:?}", alphs, alphs_);
            }
        }

        MultipleDyckLanguage{ alphabet, partition }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
enum MDTreeElem<A> {
    Root,
    Node(Option<A>, BTreeSet<A>)
}

impl<T: Eq + Hash + Clone + Ord + PartialEq + Debug> MultipleDyckLanguage<T> {
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

        
        for sigma in self.alphabet.clone().into_iter() {

            let mut next_subset: BTreeSet<T> = self.partition.get(&sigma).unwrap().iter().map(|e| e.clone()).collect();
            next_subset.remove(&sigma);
            transitions.push(Transition{
                _dummy: PhantomData,
                word: vec![Bracket::Open(sigma.clone())],
                weight: 1,
                instruction: TreeStackInstruction::NDPush{
                    lower_current: MDTreeElem::Root,
                    upper_new: MDTreeElem::Node(Some(sigma.clone()), next_subset)
                }
            });
            for subset in power(self.partition.get(&sigma).unwrap()).into_iter() {
                if subset.contains(&sigma) {
                    let mut next_subset = subset.clone();
                    next_subset.remove(&sigma);
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

            for predecessor_sigma in self.alphabet.clone().into_iter() {
                for p_subset in power(self.partition.get(&predecessor_sigma).unwrap()).into_iter() {
                    
                    if !p_subset.contains(&predecessor_sigma){
                        let mut next_subset: BTreeSet<T> = self.partition.get(&sigma).unwrap().iter().map(|e| e.clone()).collect();
                        next_subset.remove(&sigma);
                        transitions.push(Transition{
                            _dummy: PhantomData,
                            word: vec![Bracket::Open(sigma.clone())],
                            weight: 1,
                            instruction: TreeStackInstruction::NDPush{
                                lower_current: MDTreeElem::Node(Some(predecessor_sigma.clone()), p_subset.clone()),
                                upper_new: MDTreeElem::Node(Some(sigma.clone()), next_subset.clone())
                            }
                        });
                    
                        for subset in power(self.partition.get(&sigma).unwrap()).into_iter(){
                            if subset.contains(&sigma) {
                                let mut next_subset = subset.clone();
                                next_subset.remove(&sigma);
                                transitions.push(Transition{
                                    _dummy: PhantomData,
                                    word: vec![Bracket::Open(sigma.clone())],
                                    weight: 1,
                                    instruction: TreeStackInstruction::NDUp{
                                        lower_current: MDTreeElem::Node(Some(predecessor_sigma.clone()), p_subset.clone()), 
                                        upper_old: MDTreeElem::Node(None, subset.clone()),
                                        upper_new: MDTreeElem::Node(Some(sigma.clone()), next_subset.clone())
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
        use std::collections::HashMap;
        use super::Bracket::*;
        use super::MultipleDyckLanguage;
        
        let words = vec![
            vec![Open(1), Close(1), Open(2), Close(2)],
            vec![Open(1), Open(2), Close(2), Open(1), Close(1), Close(1), Open(2), Close(2)],
            vec![Open(1), Open(2), Close(2), Close(1), Open(2), Open(1), Close(1), Close(2)],
            vec![Open(1), Open(3), Close(3), Close(1), Open(2), Open(4), Close(4), Close(2)]
        ];

        let mdl = MultipleDyckLanguage::new(vec![1,2,3,4], vec![vec![1,2],vec![3,4]]);

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