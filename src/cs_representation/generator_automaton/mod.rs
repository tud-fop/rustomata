use integeriser::HashIntegeriser;
use openfsa::fsa::Automaton;
use log_domain::LogDomain;

use std::hash::Hash;

use pmcfg::PMCFGRule;
use dyck::Bracket;
use cs_representation::BracketContent;

mod naive;
mod approx;

pub enum GeneratorStrategy {
    Naive,
    Approx(usize)
}

pub type GeneratorFsa<T> = Automaton<Bracket<BracketContent<T>>>;

impl GeneratorStrategy {
    pub fn fsa<N, T>(self, rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, init: N) -> GeneratorFsa<T>
    where
        T: Clone + PartialEq + Hash + Eq + Ord,
        N: Clone + Hash + Eq + Ord
    {
        match self {
            GeneratorStrategy::Approx(depth) => approx::generator(rules, init, depth),
            GeneratorStrategy::Naive => naive::generator(rules, init)
        }
    }
}