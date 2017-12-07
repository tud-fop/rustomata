use integeriser::HashIntegeriser;
use pmcfg::PMCFGRule;
use openfsa::fsa::Automaton;
use cs_representation::bracket_fragment::BracketFragment;
use std::hash::Hash;

pub mod naive;
pub use cs_representation::filter_automaton::naive::NaiveFilterAutomaton;

pub trait FilterAutomaton<T>
where
    T: Hash + Eq + Clone
{
    fn new<N, W>(grammar: &HashIntegeriser<PMCFGRule<N, T, W>>) -> Self
    where
        N: Hash + Eq + Clone,
        W: Eq + Clone;
    fn fsa(&self, word: &[T], reference: &Automaton<BracketFragment<T>>) -> Automaton<BracketFragment<T>>;
}