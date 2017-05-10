use std::clone::*;

pub use util::*;

pub use approximation::*;
pub use cfg::*;

impl<A : Relabel<P, N1, N2, B> +Ord + Clone,
     B: Ord + Clone,
     P: Copy, N1: Clone, N2: Clone> Relabel<P, N1, N2, PushDown<B>> for PushDown<A>
    where P: Fn(N1) -> N2{
        fn relabel(&self, func: P) -> PushDown<B> {
            let mut new_elements =Vec::new();
            for el in self.elements.clone(){
                new_elements.push(el.relabel(func));
            }

            PushDown{
                elements: new_elements.clone(),
                empty: self.empty.relabel(func),
            }


        }
}

impl<N1: Clone, N2: Clone, T: Clone, P> Relabel<P, N1, N2, PushState<N2, T>> for PushState<N1, T>
    where P: Fn(N1) -> N2{
        fn relabel(&self, func: P) -> PushState<N2, T> {
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
