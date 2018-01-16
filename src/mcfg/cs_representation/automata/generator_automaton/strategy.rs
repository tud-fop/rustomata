use std::hash::Hash;
use mcfg::cs_representation::bracket_fragment::BracketFragment;
use integeriser::{HashIntegeriser, Integeriser};
use log_domain::LogDomain;
use mcfg::cs_representation::automata::{FiniteArc, FiniteAutomaton, PushDownArc, PushDownAutomaton, Operation, GeneratorAutomaton};
use pmcfg::{PMCFGRule, VarT};


use serde::{Serialize, Deserialize};

/// A `GeneratorStrategy` is a method to create a `GeneratorAutomaton` with respect to an MCFG.
pub trait GeneratorStrategy<T>
where
    T: Clone + Hash + Eq
{
    type Generator: GeneratorAutomaton<BracketFragment<T>> + Serialize + for<'de> Deserialize<'de>;
    
    fn create_generator_automaton<N>(&self, grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>, initial: N) -> Self::Generator
    where
        N: Hash + Ord + Clone;
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
    
    fn create_generator_automaton<N>(&self, grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>, initial: N) -> Self::Generator
    where
        N: Hash + Ord + Clone
    {
        let (stack_transitions, start, stop) = mcfg_to_stack_transitions(grammar, initial);
        PushDownAutomaton::new(stack_transitions, start, vec![stop])
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
    
    fn create_generator_automaton<N>(&self, grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>, initial: N) -> Self::Generator
    where
        N: Hash + Ord + Clone
    {
        let &ApproxGenerator(depth) = self;
        let (stack_transitions, start, stop) = mcfg_to_stack_transitions(grammar, initial);
        PushDownAutomaton::new(stack_transitions, start, vec![stop]).approximate(depth)
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
    
    fn create_generator_automaton<N>(&self, grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>, initial: N) -> Self::Generator
    where
        N: Hash + Ord + Clone
    {
        let (stack_transitions, start, stop) = mcfg_to_stack_transitions(grammar, initial);
        let finite_transitions: Vec<_> = stack_transitions.into_iter().map(| PushDownArc{ from, to, weight, label, ..} | FiniteArc{ from, to, weight, label }).collect();
        FiniteAutomaton::new(finite_transitions, start, vec![stop])
    }
}

use mcfg::cs_representation::BracketContent;
use dyck::Bracket;
type State<N> = Bracket<(N, usize)>;
type PushDownElem = (usize, usize, usize);

/// Reads off `PushDownArcs` from an MCFG.
fn mcfg_to_stack_transitions<N, T>(
    rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>,
    initial: N,
) -> (
    Vec<PushDownArc<BracketFragment<T>, State<N>, PushDownElem, LogDomain<f64>>>,
    State<N>,
    State<N>,
)
where
    N: Clone + Hash + Eq,
    T: Clone + Hash + Eq,
{
    let mut transitions = Vec::new();
    for rule_id in 0..rules.size() {
        let rule = rules.find_value(rule_id).unwrap();
        let weight_per_comp = rule.weight
            .pow(1f64 / (rule.composition.composition.len() as f64));
        for (component_id, component) in rule.composition.composition.iter().enumerate() {
            let weight_per_transition = weight_per_comp
                .pow(1f64 / (component.iter().filter(|s| s.is_var()).count() + 1) as f64);
            let mut terminals = vec![
                Bracket::Open(BracketContent::Component(rule_id, component_id)),
            ];
            let mut stackop: Option<PushDownElem> = None;
            let mut q = Bracket::Open((rule.head.clone(), component_id));

            for (symbol_id, symbol) in component.into_iter().enumerate() {
                match *symbol {
                    VarT::T(ref t) => {
                        terminals.push(Bracket::Open(BracketContent::Terminal(t.clone())));
                        terminals.push(Bracket::Close(BracketContent::Terminal(t.clone())));
                    }
                    VarT::Var(i, j) => {
                        terminals.push(Bracket::Open(BracketContent::Variable(rule_id, i, j)));
                        transitions.push(PushDownArc {
                            from: q,
                            to: Bracket::Open((rule.tail[i].clone(), j)),
                            label: BracketFragment(terminals),
                            op: if let Some(e) = stackop {
                                Operation::Replace(e, (rule_id, component_id, symbol_id))
                            } else {
                                Operation::Add((rule_id, component_id, symbol_id))
                            },
                            weight: weight_per_transition,
                        });
                        terminals = vec![Bracket::Close(BracketContent::Variable(rule_id, i, j))];
                        stackop = Some((rule_id, component_id, symbol_id));
                        q = Bracket::Close((rule.tail[i].clone(), j));
                    }
                }
            }
            terminals.push(Bracket::Close(
                BracketContent::Component(rule_id, component_id),
            ));
            transitions.push(PushDownArc {
                from: q,
                to: Bracket::Close((rule.head.clone(), component_id)),
                label: BracketFragment(terminals),
                op: if let Some(e) = stackop {
                    Operation::Remove(e)
                } else {
                    Operation::Nothing
                },
                weight: weight_per_transition,
            });
        }
    }

    (
        transitions,
        Bracket::Open((initial.clone(), 0)),
        Bracket::Close((initial, 0)),
    )
}
