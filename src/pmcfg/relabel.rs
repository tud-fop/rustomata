use std::hash::Hash;

pub use util::*;
pub use approximation::equivalence_classes::EquivalenceClass;

pub use approximation::relabel::*;
pub use pmcfg::*;

impl<N1: Clone + Eq + Hash, N2: Clone + Eq + Hash, T: Clone, W: Clone> Relabel<N1, N2, PMCFGRule<N2, T, W>> for PMCFGRule<N1, T, W>{
        fn relabel(&self, mapping: &EquivalenceClass<N1, N2>) -> PMCFGRule<N2, T, W> {
            let mut new_tail = Vec::new();
            for t in self.tail.clone(){
                new_tail.push(mapping.project(&t).clone());
            }

            PMCFGRule {
                head: mapping.project(&self.head).clone(),
                tail: new_tail,
                composition: self.composition.clone(),
                weight: self.weight.clone(),
            }
        }
}
