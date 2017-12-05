use integeriser::{HashIntegeriser, Integeriser};
use log_domain::LogDomain;
use pmcfg::PMCFGRule;
use dyck::Bracket;
use cs_representation::BracketContent;
use std::hash::Hash;
use pmcfg::VarT;
use openfsa::fsa::{Automaton, Arc};

use super::{GeneratorAutomaton, Delta};

/// The naive implementation of a Generator automaton.
pub struct NaiveGeneratorAutomaton;

impl GeneratorAutomaton for NaiveGeneratorAutomaton {
    fn convert<T, N>(&self, rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, initial: N) -> Automaton<Delta<T>>
    where
        N: Hash + Eq + Clone,
        T: Hash + Eq + Clone
    {
        let mut arcs = Vec::new();

        for rule_id in 0..(rules.size()) {
            let rule = rules.find_value(rule_id).unwrap();
            let comp_prob = rule.weight.pow(
                1f32 / rule.composition.composition.len() as f32,
            );
            for (component, composition) in rule.composition.composition.iter().enumerate() {
                let rule_prob = comp_prob.pow( 
                    1f32 / ((composition.iter().filter(|x| x.is_var()).count() + 1) as f32)
                );
                let mut terminals = vec![Bracket::Open(BracketContent::Component(rule_id, component))];
                let mut state = Bracket::Open((rule.head.clone(), component));
                for symbol in composition {
                    match *symbol {
                        VarT::T(ref t) => {
                            terminals.push(Bracket::Open(BracketContent::Terminal(t.clone())));
                            terminals.push(Bracket::Close(BracketContent::Terminal(t.clone())));
                        }
                        VarT::Var(i, j) => {
                            terminals.push(Bracket::Open(BracketContent::Variable(rule_id, i, j)));
                            arcs.push(Arc::new(
                                state,
                                Bracket::Open((rule.tail[i].clone(), j)),
                                terminals,
                                rule_prob,
                            ).unwrap());

                            state = Bracket::Close((rule.tail[i].clone(), j));
                            terminals = vec![Bracket::Close(BracketContent::Variable(rule_id, i, j))];
                        }
                    }
                }
                terminals.push(Bracket::Close(BracketContent::Component(rule_id, component)));

                arcs.push(Arc::new(
                    state,
                    Bracket::Close((rule.head.clone(), component)),
                    terminals,
                    rule_prob,
                ).unwrap());
            }
        }
        
        Automaton::from_arcs(
            Bracket::Open((initial.clone(), 0)),
            vec![Bracket::Close((initial, 0))],
            arcs,
        )
    }
}