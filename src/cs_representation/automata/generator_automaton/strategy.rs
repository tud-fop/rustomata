use std::hash::Hash;
use cs_representation::bracket_fragment::BracketFragment;
use integeriser::{HashIntegeriser, Integeriser};
use log_domain::LogDomain;
use cs_representation::automata::{FiniteArc, FiniteAutomaton, KellerArc, KellerAutomaton, KellerOp, GeneratorAutomaton};
use pmcfg::{PMCFGRule, VarT};

use serde::{Serialize, Deserialize};

///
pub trait GeneratorStrategy<T>
where
    T: Clone + Hash + Eq
{
    type Generator: GeneratorAutomaton<BracketFragment<T>> + Serialize + for<'de> Deserialize<'de>;
    
    fn create_generator_automaton<N>(&self, grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, initial: N) -> Self::Generator
    where
        N: Hash + Ord + Clone;
}

pub struct KellerGenerator;
impl<T> GeneratorStrategy<T> for KellerGenerator
where
    T: Clone + Hash + Eq + Serialize + for<'de> Deserialize<'de>
{
    type Generator = KellerAutomaton<BracketFragment<T>>;
    
    fn create_generator_automaton<N>(&self, grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, initial: N) -> KellerAutomaton<BracketFragment<T>>
    where
        N: Hash + Ord + Clone
    {
        let (stack_transitions, start, stop) = mcfg_to_stack_transitions(grammar, initial);
        KellerAutomaton::new(stack_transitions, start, vec![stop])
    }
}

pub struct ApproxGenerator(pub usize);
impl<T> GeneratorStrategy<T> for ApproxGenerator
where
    T: Clone + Hash + Eq + Serialize + for<'de> Deserialize<'de>
{
    type Generator = FiniteAutomaton<BracketFragment<T>>;
    fn create_generator_automaton<N>(&self, grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, initial: N) -> FiniteAutomaton<BracketFragment<T>>
    where
        N: Hash + Ord + Clone
    {
        let &ApproxGenerator(depth) = self;
        let (stack_transitions, start, stop) = mcfg_to_stack_transitions(grammar, initial);
        KellerAutomaton::new(stack_transitions, start, vec![stop]).approximate(depth)
    }
}

pub struct NaiveGenerator;
impl<T> GeneratorStrategy<T> for NaiveGenerator
where
    T: Hash + Eq + Clone + Serialize + for<'de> Deserialize<'de>
{
    type Generator = FiniteAutomaton<BracketFragment<T>>;
    
    fn create_generator_automaton<N>(&self, grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, initial: N) -> FiniteAutomaton<BracketFragment<T>>
    where
        N: Hash + Ord + Clone
    {
        let (stack_transitions, start, stop) = mcfg_to_stack_transitions(grammar, initial);
        let finite_transitions: Vec<_> = stack_transitions.into_iter().map(| KellerArc{ from, to, weight, label, ..} | FiniteArc{ from, to, weight, label }).collect();
        FiniteAutomaton::new(finite_transitions, start, vec![stop])
    }
}

use cs_representation::BracketContent;
use dyck::Bracket;
type State<N> = Bracket<(N, usize)>;
type KellerElem = (usize, usize, usize);

fn mcfg_to_stack_transitions<N, T>(
    rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>,
    initial: N,
) -> (
    Vec<KellerArc<BracketFragment<T>, State<N>, KellerElem>>,
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
            .pow(1f32 / (rule.composition.composition.len() as f32));
        for (component_id, component) in rule.composition.composition.iter().enumerate() {
            let weight_per_transition = weight_per_comp
                .pow(1f32 / (component.iter().filter(|s| s.is_var()).count() + 1) as f32);
            let mut terminals = vec![
                Bracket::Open(BracketContent::Component(rule_id, component_id)),
            ];
            let mut stackop: Option<KellerElem> = None;
            let mut q = Bracket::Open((rule.head.clone(), component_id));

            for (symbol_id, symbol) in component.into_iter().enumerate() {
                match symbol {
                    &VarT::T(ref t) => {
                        terminals.push(Bracket::Open(BracketContent::Terminal(t.clone())));
                        terminals.push(Bracket::Close(BracketContent::Terminal(t.clone())));
                    }
                    &VarT::Var(i, j) => {
                        terminals.push(Bracket::Open(BracketContent::Variable(rule_id, i, j)));
                        transitions.push(KellerArc {
                            from: q,
                            to: Bracket::Open((rule.tail[i].clone(), j)),
                            label: BracketFragment(terminals),
                            op: if let Some(e) = stackop {
                                KellerOp::Replace(e, (rule_id, component_id, symbol_id))
                            } else {
                                KellerOp::Add((rule_id, component_id, symbol_id))
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
            transitions.push(KellerArc {
                from: q,
                to: Bracket::Close((rule.head.clone(), component_id)),
                label: BracketFragment(terminals),
                op: if let Some(e) = stackop {
                    KellerOp::Remove(e)
                } else {
                    KellerOp::Nothing
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
