extern crate num_traits;

use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;
use std::vec::Vec;
use std::ops::{AddAssign, Mul, Div};
use self::num_traits::{One, Zero};
use std::str::FromStr;

use recognisable::Transition;
use grammars::cfg::*;
use automata::push_down_automaton::{PushDown, PushDownAutomaton, PushDownInstruction};

/// Symbols of a `PushDown` created by an `CFG`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum PushState<X, Y> {
    Designated,
    Initial,
    Nt(X),
    T(Y),
}

impl<A, Y> PushState<A, Y>
where
    Y: Clone,
{
    pub fn map<F, B>(&self, f: F) -> PushState<B, Y>
    where
        F: Fn(&A) -> B,
    {
        match *self {
            PushState::Designated => PushState::Designated,
            PushState::Initial => PushState::Initial,
            PushState::Nt(ref a) => PushState::Nt(f(a)),
            PushState::T(ref x) => PushState::T(x.clone()),
        }
    }
}

impl<X: fmt::Display, Y: fmt::Display> fmt::Display for PushState<X, Y> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PushState::Designated => write!(f, "@"),
            PushState::Initial => write!(f, "I"),
            PushState::Nt(ref x) => write!(f, "({})", x),
            PushState::T(ref x) => write!(f, "({})", x),
        }
    }
}

impl<N: Clone + Ord + PartialEq + Hash,
     T: Clone + Ord + PartialEq + Hash,
     W: Clone + Ord + PartialEq + One + FromStr + AddAssign + Mul<Output = W> + Div<Output = W> + Zero
     > From<CFG<N, T, W>> for PushDownAutomaton<PushState<N,T>, T, W>
{
     fn from(g: CFG<N, T, W>) -> Self {
        let mut transitions = Vec::new();

        let mut t_buffer= HashSet::new();

// creates a Transition for every rule that replaces the nonterminal of the left side with the rightside of the rule
        for r in g.rules.clone(){
            let mut st = Vec::new();
            for v in r.composition.composition{

                match v {
                    LetterT::Value(x) => {
                        t_buffer.insert(x.clone());
                        st.insert(0,PushState::T(x.clone()));
                    },
                    LetterT::Label(x) => {
                        st.insert(0,PushState::Nt(x.clone()));
                    },
                }
            }

            transitions.push(
                Transition {
                    word: Vec::new(),
                    weight: r.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![PushState::Nt(r.head.clone())],
                        new_val: st.clone(),
                    }
                }
            );

        }
// creates a transition for every terminal that reads the word
        for t in t_buffer{
            let mut tvec = Vec::new();
            tvec.push(t.clone());
            transitions.push(
                Transition {
                    word: tvec.clone(),
                    weight: W::one(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![PushState::T(t.clone())],
                        new_val: Vec::new(),
                    }
                }
            );


        }

// creates a transition for the `Initial` symbol to all Nonterminals in `initial` with weight `one`
        for ini in g.initial{
            let mut tvec = Vec::new();
            tvec.push(PushState::Nt(ini));
            transitions.push(
                Transition {
                    word: Vec::new(),
                    weight: W::one(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![PushState::Initial],
                        new_val: tvec,
                    }
                }
            );
        }

        PushDownAutomaton::new(
            transitions,
            PushDown::new(PushState::Designated, PushState::Initial),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_state_map_correctness() {
        let inputs = vec![
            (PushState::Designated, PushState::Designated),
            (PushState::Initial, PushState::Initial),
            (PushState::Nt(1), PushState::Nt(2)),
            (PushState::T('a'), PushState::T('a')),
        ];

        for (input, control_output) in inputs {
            assert_eq!(control_output, input.map(&|x: &u8| x * 2));
        }
    }

    #[test]
    fn test_push_state_map_inverse() {
        let state: PushState<u8, char> = PushState::Nt(1);
        let mapped_state = state.map(&|x: &u8| x * 2);

        assert_eq!(state, mapped_state.map(&|x: &u8| x / 2));
    }
}
