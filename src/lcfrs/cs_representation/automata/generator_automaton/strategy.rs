use std::hash::Hash;
use lcfrs::cs_representation::bracket_fragment::BracketFragment;
use integeriser::{Integeriser};
use log_domain::LogDomain;
use lcfrs::cs_representation::automata::{FiniteAutomaton, PushDownAutomaton, GeneratorAutomaton};
use pmcfg::{PMCFGRule};
use super::super::super::rule_fragments::fragments;
use dyck::Bracket;

use serde::{Serialize, Deserialize};

/// A `GeneratorStrategy` is a method to create a `GeneratorAutomaton` with respect to an MCFG.
pub trait GeneratorStrategy<T>
where
    T: Clone + Hash + Eq
{
    type Generator: GeneratorAutomaton<BracketFragment<T>> + Serialize + for<'de> Deserialize<'de>;
    
    fn create_generator_automaton<'a, N, R>(&self, grammar_rules: R, initial: N, integeriser: &Integeriser<Item=PMCFGRule<N, T, LogDomain<f64>>>) -> Self::Generator
    where
        N: Hash + Ord + Clone + 'a,
        R: IntoIterator<Item=&'a PMCFGRule<N, T, LogDomain<f64>>>,
        T: 'a;
}

/// A `PushDownGenerator` is a `GeneratorStrtegy` that creates a `PushDownAutomaton`
/// from an MCFG. In this implementation, it is the most specialized
/// strategy. I.e. it produces the least amount of canidates.
pub struct PushDownGenerator;
impl<T> GeneratorStrategy<T> for PushDownGenerator
where
    T: Clone + Hash + Eq + Serialize + for<'de> Deserialize<'de>
{
    type Generator = PushDownAutomaton<BracketFragment<T>, LogDomain<f64>>;
    
    fn create_generator_automaton<'a, N, R>(&self, grammar_rules: R, initial: N, integeriser: &Integeriser<Item=PMCFGRule<N, T, LogDomain<f64>>>) -> Self::Generator
    where
        N: Hash + Ord + Clone + 'a,
        T: 'a,
        R: IntoIterator<Item=&'a PMCFGRule<N, T, LogDomain<f64>>>
    {
        let mut transitions = Vec::new();
        for rule in grammar_rules {
            transitions.extend(fragments(rule).map(|f| f.pds(integeriser)));
        }
        PushDownAutomaton::new(transitions, Bracket::Open((initial.clone(), 0)), vec![Bracket::Close((initial, 0))])
    }
}

/// An `ApproxGenerator` is a hybrid between the `PushDownGenerator` and `NaiveGenerator`.
/// It produces a deterministic finite approximation of the `PushDownAutomaton` 
/// produced by the `PushDownGenerator` with respect to a certain depth.
/// The `FiniteAutomaton` produced by `ApproxGenerator(0)` equals the one by `NaiveGenerator`.
pub struct ApproxGenerator(pub usize);
impl<T> GeneratorStrategy<T> for ApproxGenerator
where
    T: Clone + Hash + Eq + Serialize + for<'de> Deserialize<'de>
{
    type Generator = FiniteAutomaton<BracketFragment<T>, LogDomain<f64>>;
    
    fn create_generator_automaton<'a, N, R>(&self, grammar_rules: R, initial: N, integeriser: &Integeriser<Item=PMCFGRule<N, T, LogDomain<f64>>>) -> Self::Generator
    where
        N: Hash + Ord + Clone + 'a,
        T: 'a,
        R: IntoIterator<Item=&'a PMCFGRule<N, T, LogDomain<f64>>>
    {
        PushDownGenerator.create_generator_automaton(grammar_rules, initial, integeriser).approximate(self.0)
    }
}

/// The `NaiveGenerator` is the least specialied strategy.
/// The `FiniteAutomaton` produced with respect to an MCFG will
/// recognize the most bracket word candidates from the three implemented strategies.
pub struct NaiveGenerator;
impl<T> GeneratorStrategy<T> for NaiveGenerator
where
    T: Hash + Eq + Clone + Serialize + for<'de> Deserialize<'de>
{
    type Generator = FiniteAutomaton<BracketFragment<T>, LogDomain<f64>>;
    
    fn create_generator_automaton<'a, N, R>(&self, grammar_rules: R, initial: N, integeriser: &Integeriser<Item=PMCFGRule<N, T, LogDomain<f64>>>) -> Self::Generator
    where
        N: Hash + Ord + Clone + 'a,
        T: 'a,
        R: IntoIterator<Item=&'a PMCFGRule<N, T, LogDomain<f64>>>
    {
        PushDownGenerator.create_generator_automaton(grammar_rules, initial, integeriser).approximate(0)
    }
}