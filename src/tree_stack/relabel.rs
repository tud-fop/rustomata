use std::clone::*;
use std::collections::HashMap;
use std::hash::Hash;

pub use util::*;
pub use util::equivalence_classes::*;

pub use approximation::relabel::*;
pub use cfg::*;
pub use push_down::*;

impl<A : Relabel<N1, N2, B> +Ord + Clone,
     B: Ord + Clone,
     N1: Clone + Eq + Hash, N2: Clone + Eq + Hash> Relabel<N1, N2, TreeStack<B>> for TreeStack<A>{
        fn relabel(&self, mapping: &EquivalenceClass<N1, N2>) -> TreeStack<B> {
            let mut new_tree: HashMap<Vec<u8>, B> = HashMap::new();
            for (p, v) in self.tree.clone(){
                new_tree.insert(p, v.relabel(mapping));
            }

            TreeStack{
                tree: new_tree,
                pointer: self.pointer.clone(),
            }
        }
}

impl<A : Relabel<N1, N2, B> +Ord + Clone,
     B: Ord + Clone,
     N1: Clone + Eq + Hash, N2: Clone + Eq + Hash> Relabel<N1, N2, PosState<B>> for PosState<A>{
        fn relabel(&self, mapping: &EquivalenceClass<N1, N2>) -> PosState<B> {
            match self {
                &PosState::Position(ref x, i, j) => {
                    PosState::Position(x.relabel(mapping), i, j)
                }
                &PosState::Designated => {
                    PosState::Designated
                }
                &PosState::Initial => {
                    PosState::Initial
                }
            }
        }
}