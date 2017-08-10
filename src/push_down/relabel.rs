use std::clone::*;

pub use util::*;
pub use util::equivalence_classes::*;

pub use approximation::relabel::*;
pub use cfg::*;
pub use push_down::*;

impl<A : Relabel<N1, N2, B> +Ord + Clone,
     B: Ord + Clone,
     N1: Clone + Eq + Hash, N2: Clone + Eq + Hash> Relabel<N1, N2, PushDown<B>> for PushDown<A>{
        fn relabel(&self, mapping: &EquivalenceClass<N1, N2>) -> PushDown<B> {
            let mut new_elements =Vec::new();
            for el in self.elements.clone(){
                new_elements.push(el.relabel(mapping));
            }
            PushDown{
                elements: new_elements,
                empty: self.empty.relabel(mapping),
            }


        }
}

impl<N1: Clone + Eq + Hash, N2: Clone + Eq + Hash, T: Clone> Relabel<N1, N2, PushState<N2, T>> for PushState<N1, T>{
        fn relabel(&self, mapping: &EquivalenceClass<N1, N2>) -> PushState<N2, T> {
            match self {
                &PushState::Nt( ref x) => {
                    PushState::Nt(mapping.project(x.clone()).clone())
                }
                &PushState::T(ref x) => {
                    PushState::T(x.clone())
                }
                &PushState::Initial => {
                    PushState::Initial
                }
                &PushState::Designated => {
                    PushState::Designated
                }
            }


        }
}