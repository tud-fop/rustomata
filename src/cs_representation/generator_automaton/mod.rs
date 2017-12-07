use integeriser::HashIntegeriser;
use openfsa::fsa::Automaton;
use log_domain::LogDomain;

use std::hash::Hash;

use pmcfg::PMCFGRule;
use dyck::Bracket;
use cs_representation::BracketContent;
use cs_representation::bracket_fragment::BracketFragment;
pub mod naive;
pub mod approx;

pub use cs_representation::generator_automaton::naive::NaiveGeneratorAutomaton;
pub use cs_representation::generator_automaton::approx::ApproxGeneratorAutomaton;

pub trait GeneratorAutomaton {
    /// Creates a Generator automaton from a set of integerised rules.
    fn convert<T, N>(&self, rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, initial: N) -> Automaton<BracketFragment<T>>
    where
        T: Eq + Hash + Clone + Ord,
        N: Eq + Hash + Clone + Ord;
}

/// The alphabet Δ(Σ) with respect to a grammar (N, Σ, P, S) consisting of three types of brackets:
/// * ⟨_σ and ⟩_σ for each σ ∈ Σ,
/// * ⟨_{p, j} and ⟩_{p, j} for each p ∈ P and j ∈ fanout(p)
/// * ⟨^{i}_{p, j} and ⟩^i_{p, j} for each p ∈ P, i ∈ rank(p) and j ∈ fanout_i(p).
pub type Delta<T> = Bracket<BracketContent<T>>;
/// A State in a Generator automaton corresponds to the start or end of a component produced by a nonterminal.
pub type State<N> = Bracket<(N, usize)>;
