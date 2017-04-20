extern crate num_traits;

use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use self::num_traits::One;
use std::marker::PhantomData;

use automata;
use cfg;
use push_down::{PushDown, PushDownAutomaton, PushDownInstruction};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum PushState<X,Y> {
    Designated,
    Nt(X),
    T(Y),
}

impl<N: Clone + Debug + Ord + PartialEq + Hash,
     T: Clone + Debug + Ord + PartialEq + Hash,
     W: Clone + Debug + Ord + PartialEq + One
     > From<cfg::CFG<N, T, W>> for PushDownAutomaton<PushState<N,T>, T, W> {
     fn from(g: cfg::CFG<N, T, W>) -> Self {
        let mut transitions = Vec::new();

        let mut t_buffer= HashSet::new();

        for r in g.rules.clone(){
            let mut st = Vec::new();
            for v in r.composition.composition{

                match v{
                    cfg::VarT::Value(x) => {
                        t_buffer.insert(x.clone());
                        st.push(PushState::T(x.clone()));
                    },
                    cfg::VarT::Label(x) => {
                        st.push(PushState::Nt(x.clone()));
                    },
                }
            }

            transitions.push(
                automata::Transition {
                    _dummy: PhantomData,
                    word: Vec::new(),
                    weight: r.weight.clone(),
                    instruction: PushDownInstruction::Stay {
                        current_val: PushState::Nt(r.head.clone()),
                        new_val: st.clone(),
                    }
                }
            );

        }

        for t in t_buffer{
            let mut tvec = Vec::new();
            tvec.push(t.clone());
            transitions.push(
                automata::Transition {
                    _dummy: PhantomData,
                    word: tvec.clone(),
                    weight: W::one(),
                    instruction: PushDownInstruction::Pop {
                        current_val: PushState::T(t.clone()),
                    }
                }
            );


        }

        PushDownAutomaton::new(
            transitions,
            PushDown::new(PushState::Designated, PushState::Nt(g.initial.clone())),
        )
    }
}
