use integeriser::{Integeriser, HashIntegeriser};
use std::hash::Hash;
use dyck::Bracket;
use grammars::lcfrs::cs_representation::BracketContent;
use std::rc::Rc;
use util::agenda::Capacity;

use super::super::rule_fragments::fragments;
use super::super::bracket_fragment::BracketFragment;
use super::{PushDownAutomaton, FiniteAutomaton};
use grammars::pmcfg::PMCFGRule;

mod cyk_generator;
use self::cyk_generator::CykGenerator;
use util::factorizable::Factorizable;
use num_traits::{One, Zero};
use std::ops::Mul;

pub enum GeneratorStrategy {
    Finite,
    Approx(usize),
    PushDown,
    CykLike,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Generator<T, W>
where
    T: Clone + Eq + Hash,
{
    Finite(FiniteAutomaton<BracketFragment<T>, W>),
    PushDown(PushDownAutomaton<BracketFragment<T>, W>),
    Cyk(FiniteAutomaton<BracketFragment<T>, W>),
}

impl<T, W> Generator<T, W>
where
    T: Clone + Eq + Hash,
{
    fn unboxed_push_down<'a, N, R>(
        grammar_rules: R,
        initial: N,
        integeriser: &Integeriser<Item = PMCFGRule<N, T, W>>,
    ) -> PushDownAutomaton<BracketFragment<T>, W>
    where
        R: IntoIterator<Item = &'a PMCFGRule<N, T, W>>,
        N: Eq + Clone + Hash + 'a,
        T: 'a,
        W: 'a + Factorizable + One + Copy
    {
        let mut transitions = Vec::new();
        for rule in grammar_rules {
            transitions.extend(fragments(&rule).map(|f| f.pds(integeriser)));
        }

        PushDownAutomaton::new(
            transitions,
            Bracket::Open((initial.clone(), 0)),
            vec![Bracket::Close((initial, 0))],
        )
    }

    pub fn push_down<'a, N, R>(
        grammar_rules: R,
        initial: N,
        integeriser: &Integeriser<Item = PMCFGRule<N, T, W>>,
    ) -> Self
    where
        R: IntoIterator<Item = &'a PMCFGRule<N, T, W>>,
        N: Eq + Clone + Hash + 'a,
        T: 'a,
        W: 'a + One + Factorizable + Copy
    {
        Generator::PushDown(Generator::unboxed_push_down(
            grammar_rules,
            initial,
            integeriser,
        ))
    }

    pub fn naive<'a, N, R>(
        grammar_rules: R,
        initial: N,
        integeriser: &Integeriser<Item = PMCFGRule<N, T, W>>,
    ) -> Self
    where
        R: IntoIterator<Item = &'a PMCFGRule<N, T, W>>,
        N: Eq + Clone + Hash + 'a,
        T: 'a,
        W: 'a + One + Factorizable + Copy
    {
        Generator::Finite(
            Generator::unboxed_push_down(grammar_rules, initial, integeriser).approximate(0),
        )
    }

    pub fn approx<'a, N, R>(
        grammar_rules: R,
        initial: N,
        integeriser: &Integeriser<Item = PMCFGRule<N, T, W>>,
        d: usize,
    ) -> Self
    where
        R: IntoIterator<Item = &'a PMCFGRule<N, T, W>>,
        N: Eq + Clone + Hash + 'a,
        T: 'a,
        W: 'a + One + Factorizable + Copy
    {
        Generator::Finite(
            Generator::unboxed_push_down(grammar_rules, initial, integeriser).approximate(d),
        )
    }

    pub fn cyk<'a, N, R>(
        grammar_rules: R,
        initial: N,
        integeriser: &Integeriser<Item = PMCFGRule<N, T, W>>,
    ) -> Self
    where
        R: IntoIterator<Item = &'a PMCFGRule<N, T, W>>,
        N: Eq + Clone + Hash + 'a,
        T: 'a,
        W: 'a + Copy + One + Factorizable
    {
        Generator::Cyk(
            Generator::unboxed_push_down(grammar_rules, initial, integeriser).approximate(0),
        )
    }

    pub fn intersect(&self, other: FiniteAutomaton<BracketFragment<T>, ()>) -> Self
    where
        W: Copy
    {
        match *self {
            Generator::Finite(ref fsa) => Generator::Finite(fsa.intersect(&other)),
            Generator::Cyk(ref fsa) => Generator::Cyk(fsa.intersect(&other)),
            Generator::PushDown(ref pda) => Generator::PushDown(pda.clone().intersect(&other)),
        }
    }

    pub fn generate<'a>(self, beam: Capacity) -> Box<Iterator<Item = Vec<Delta<T>>> + 'a>
    where
        T: 'a,
        W: 'a +  Copy + Ord + One + Zero + Mul<Output=W>
    {
        match self {
            Generator::Finite(fsa) => Box::new(
                fsa.generate(beam).map(|fs| BracketFragment::concat(fs)),
            ),
            Generator::PushDown(pda) => Box::new(
                pda.generate(beam).map(|fs| BracketFragment::concat(fs)),
            ),
            Generator::Cyk(fsa) => Box::new(CykGenerator::new(fsa)),
        }
    }

    pub fn get_integeriser(&self) -> Rc<HashIntegeriser<BracketFragment<T>>> {
        match *self {
            Generator::Finite(ref fsa) => fsa.get_integeriser(),
            Generator::PushDown(ref pda) => pda.get_integeriser(),
            Generator::Cyk(ref fsa) => fsa.get_integeriser(),
        }
    }

    pub fn size(&self) -> usize {
        match *self {
            Generator::Finite(ref fsa) => fsa.size(),
            Generator::Cyk(ref fsa) => fsa.size(),
            Generator::PushDown(ref pda) => pda.size(),
        }
    }
}

/// The alphabet Δ(G) with respect to a grammar G = (N, Σ, P, S) consisting of three types of brackets:
/// * ⟨_σ and ⟩_σ for each σ ∈ Σ,
/// * ⟨_{p, j} and ⟩_{p, j} for each p ∈ P and j ∈ fanout(p)
/// * ⟨^{i}_{p, j} and ⟩^i_{p, j} for each p ∈ P, i ∈ rank(p) and j ∈ fanoutᵢ(p).
pub type Delta<T> = Bracket<BracketContent<T>>;
