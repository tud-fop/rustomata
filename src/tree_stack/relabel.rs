use std::clone::*;

pub use util::*;

pub use approximation::relabel::*;
pub use cfg::*;
pub use push_down::*;

impl<A : Relabel<N1, N2, B> +Ord + Clone,
     B: Ord + Clone,
     N1: Clone, N2: Clone> Relabel<N1, N2, TreeStack<B>> for TreeStack<A>{
        fn relabel(&self, func: fn(N1)-> N2) -> TreeStack<B> {
            let mut new_tree: HashMap<Vec<u8>, A> = HashMap::new();
            for (p, v) in self.tree{
                tree.insert(p, v.relabel(func));
            }

            PushDown{
                tree: new_tree,
                pointer: self.pointer.clone(),
            }
        }
}

impl<N1: Clone, N2: Clone, T: Clone> Relabel<N1, N2, PosState<N2>> for PosState<N1>{
        fn relabel(&self, func: fn(N1)-> N2) -> PosState<N2> {
            match self {
                &PosState::Position(ref x, i, j) => {
                    PosState::Position(func(x.clone()), i, j)
                }
                &PosState::Designated => {
                    PosState::Designated
                }
            }


        }
}
