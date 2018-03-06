use integeriser::HashIntegeriser;
use std::hash::Hash;
use dyck::Bracket;
use lcfrs::cs_representation::BracketContent;
use super::FiniteAutomaton;
use std::rc::Rc;
use util::agenda::Capacity;

pub mod strategy;
pub use self::strategy::{GeneratorStrategy, PushDownGenerator, NaiveGenerator, ApproxGenerator};

/// A `GeneratorAutomaton` with respect to a weighted LCFRS is an automaton
/// that recognizes bracket words δ ∈ Δ(G).
pub trait GeneratorAutomaton<T>
where
    T: Hash + Clone + Eq
{
    /// Size of the automaton (the number of transitions) for debugging purposes.
    fn size(&self) -> usize;
    
    /// Returns a pointer to the internal `Integeriser` that is used to integerize `BracketFragements`.
    fn get_integeriser(&self) -> Rc<HashIntegeriser<T>>;

    /// Constructs the intersection of the automaton with an unweighted finite state automaton.
    fn intersect(&self, other: FiniteAutomaton<T, ()>) -> Self
    where Self: Sized;

    /// Returns an `Iterator` that enumerates words recognized by this automaton.
    /// If `beam` is a `Capacity::Limit`, it uses beam search.
    fn generate<'a>(self, beam: Capacity) -> Box<Iterator<Item=Vec<T>> + 'a>
    where T: 'a;
}

/// The alphabet Δ(G) with respect to a grammar G = (N, Σ, P, S) consisting of three types of brackets:
/// * ⟨_σ and ⟩_σ for each σ ∈ Σ,
/// * ⟨_{p, j} and ⟩_{p, j} for each p ∈ P and j ∈ fanout(p)
/// * ⟨^{i}_{p, j} and ⟩^i_{p, j} for each p ∈ P, i ∈ rank(p) and j ∈ fanoutᵢ(p).
pub type Delta<T> = Bracket<BracketContent<T>>;
