use integeriser::Integeriser;
use pmcfg::PMCFGRule;
use std::hash::Hash;
use lcfrs::cs_representation::bracket_fragment::BracketFragment;

pub mod naive;
pub mod inside;
use super::{FiniteAutomaton, GeneratorAutomaton};

pub use self::naive::NaiveFilterAutomaton;
pub use self::inside::InsideFilterAutomaton;

/// A `FilterAutomaton` is a structure that produces a `FiniteAutomaton`
/// with respect to a grammar w ∈ Σ and a word s.t.
/// it recognizes all bracket words δ ∈ Δ* that represent a 
/// derivation of the word in the grammar h(δ) = w.
/// The generation of this `FiniteAutomaton` is divided into
/// two steps; an initialization and the generation itself.
pub trait FilterAutomaton<'a, T: 'a>
where
    T: Hash + Eq + Clone,
{
    fn new<N: 'a, W: 'a, I, R>(
        grammar_rules: R,
        grammar: &I,
        reference_automaton: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> Self
    where
        N: Hash + Eq + Clone,
        W: Eq + Clone,
        R: Iterator<Item=&'a PMCFGRule<N, T, W>>,
        I: Integeriser<Item=PMCFGRule<N, T, W>>;

    fn fsa(
        &self,
        word: &[T],
        reference_automaton: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> FiniteAutomaton<BracketFragment<T>, ()>;
}