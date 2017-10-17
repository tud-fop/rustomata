use std::clone::*;
use std::hash::Hash;

use util::equivalence_classes::*;
use approximation::relabel::*;
use tree_stack::*;

impl<A: Relabel<N1, N2, B> +Ord + Clone,
     B: Ord + Clone,
     N1: Clone + Eq + Hash, N2: Clone + Eq + Hash> Relabel<N1, N2, TreeStack<B>> for TreeStack<A>{
        fn relabel(&self, mapping: &EquivalenceClass<N1, N2>) -> TreeStack<B> {
            self.map(&|v| v.relabel(mapping))
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
