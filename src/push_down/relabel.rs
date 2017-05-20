use std::clone::*;

pub use util::*;

pub use approximation::relabel::*;
pub use cfg::*;
pub use push_down::*;

impl<A : Relabel<N1, N2, B> +Ord + Clone,
     B: Ord + Clone,
     N1: Clone, N2: Clone> Relabel<N1, N2, PushDown<B>> for PushDown<A>{
        fn relabel(&self, func: fn(N1)-> N2) -> PushDown<B> {
            let mut new_elements =Vec::new();
            for el in self.elements.clone(){
                new_elements.push(el.relabel(func));
            }

            PushDown{
                elements: new_elements.clone(),
                limit: self.limit,
                empty: self.empty.relabel(func),
            }


        }
}

impl<N1: Clone, N2: Clone, T: Clone> Relabel<N1, N2, PushState<N2, T>> for PushState<N1, T>{
        fn relabel(&self, func: fn(N1)-> N2) -> PushState<N2, T> {
            match self {
                &PushState::Nt( ref x) => {
                    PushState::Nt(func(x.clone()))
                }
                &PushState::T(ref x) => {
                    PushState::T(x.clone())
                }
                &PushState::Designated => {
                    PushState::Designated
                }
                &PushState::Initial => {
                    PushState::Initial
                }
            }


        }
}
