use integeriser::HashIntegeriser;
use pmcfg::PMCFGRule;
use std::hash::Hash;
use cs_representation::bracket_fragment::BracketFragment;

pub mod naive;
use super::{FiniteAutomaton, GeneratorAutomaton};

pub use self::naive::NaiveFilterAutomaton;

pub trait FilterAutomaton<T>
where
    T: Hash + Eq + Clone,
{
    fn new<N, W>(
        grammar: &HashIntegeriser<PMCFGRule<N, T, W>>,
        reference_automaton: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> Self
    where
        N: Hash + Eq + Clone,
        W: Eq + Clone;

    fn fsa(
        &self,
        word: &[T],
        reference_automaton: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> FiniteAutomaton<BracketFragment<T>>;
}
