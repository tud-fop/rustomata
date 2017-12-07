use openfsa::fsa::{Automaton, Arc};
use cs_representation::{BracketContent};
use pmcfg::{PMCFGRule, VarT};
use log_domain::LogDomain;
use integeriser::HashIntegeriser;
use integeriser::Integeriser;
use std::collections::{BTreeSet, HashMap, BinaryHeap};
use std::hash::Hash;
use util::push_down::Pushdown;
use dyck::Bracket;
use recognisable::Recogniser;

use cs_representation::bracket_fragment::BracketFragment;

use super::{GeneratorAutomaton, State};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum KellerOp<S> {
    Nothing,
    Remove(S),
    Add(S),
    Replace(S, S)
}
type KellerElem = (usize, usize, usize);     // rule, component, index
type Transition<N, T> = (State<N>, BracketFragment<T>, KellerOp<KellerElem>, State<N>, LogDomain<f32>);

fn mcfg_to_stack_transitions<N, T>(
    rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>,
    initial: N
) -> (Vec<Transition<N, T>>, State<N>, State<N>)
where 
    N: Clone + PartialEq + Hash + Eq,
    T: Clone + PartialEq + Hash + Eq
{
    let mut transitions = Vec::new();
    for rule_id in 0..rules.size() {
        let rule = rules.find_value(rule_id).unwrap();
        let weight_per_comp = rule.weight.pow( 1f32 / (rule.composition.composition.len() as f32) );
        for (component_id, component) in rule.composition.composition.iter().enumerate() {
            let weight_per_transition = weight_per_comp.pow( 1f32 / (component.iter().filter(|s| s.is_var()).count() + 1) as f32 );
            let mut terminals = vec![ Bracket::Open(BracketContent::Component(rule_id, component_id)) ];
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
                        transitions.push(
                            (
                                q,
                                BracketFragment(terminals),
                                if let Some(e) = stackop {
                                    KellerOp::Replace(e, (rule_id, component_id, symbol_id))
                                } else {
                                    KellerOp::Add((rule_id, component_id, symbol_id))
                                },
                                Bracket::Open((rule.tail[i].clone(), j)),
                                weight_per_transition
                            )
                        );
                        terminals = vec![ Bracket::Close(BracketContent::Variable(rule_id, i, j)) ];
                        stackop = Some((rule_id, component_id, symbol_id));
                        q = Bracket::Close((rule.tail[i].clone(), j));
                    }
                }
            }
            terminals.push(Bracket::Close(BracketContent::Component(rule_id, component_id)));
            transitions.push(
                (
                    q,
                    BracketFragment(terminals),
                    if let Some(e) = stackop {
                        KellerOp::Remove(e)
                    } else {
                        KellerOp::Nothing
                    },
                    Bracket::Close((rule.head.clone(), component_id)),
                    weight_per_transition)
            );
        }
    }

    (transitions, Bracket::Open((initial.clone(), 0)), Bracket::Close((initial, 0)))
}

type ApproxState<N> = (State<N>, Vec<KellerElem>); 
type ApproxStackTransitionIterator<N, T> = Recogniser<'static, Vec<(Arc<ApproxState<N>, BracketFragment<T>>, Pushdown<Transition<N, T>>)>, Arc<ApproxState<N>, BracketFragment<T>>, Transition<N, T>, State<N>, Arc<ApproxState<N>, BracketFragment<T>>>;

fn stack_transitions_to_finite_transitions<N, T>(
    sa_transitions: Vec<Transition<N, T>>,
    start: &State<N>,
    depth: usize
    ) -> ApproxStackTransitionIterator<N, T>
where
    N: Clone + PartialEq + Hash + Eq + Ord,
    T: Clone + PartialEq + Ord
{
    let mut initial_agenda: Vec<(Arc<ApproxState<N>, BracketFragment<T>>, Pushdown<Transition<N, T>>)> = Vec::new();
    let mut rulemap: HashMap<State<N>, BinaryHeap<Transition<N, T>>> = HashMap::new();
    
    for (q0, word, so, q1, w) in sa_transitions {
        if q0 == *start {
            if let KellerOp::Nothing = so {
                initial_agenda.push(
                    (
                        Arc{ 
                            from: (q0.clone(), Vec::new()),
                            to: (q1.clone(), Vec::new()),
                            label: word.clone(),
                            weight: w
                        },
                        Pushdown::new()
                    )
                );
            } else if let KellerOp::Add(e) = so {
                let mut stack = vec![e];
                stack.truncate(depth);
                initial_agenda.push(
                    (
                        Arc{ 
                            from: (q0.clone(), Vec::new()),
                            to: (q1.clone(), stack),
                            label: word.clone(),
                            weight: w
                        },
                        Pushdown::new()
                    )
                );
            }
        }
        rulemap.entry(q0.clone()).or_insert(BinaryHeap::new()).push((q0, word, so, q1, w));
    }

    Recogniser{
        agenda: initial_agenda,
        configuration_characteristic: Box::new( |c| {
            let (ref a, _) = c.to;
            a
        }),
        filtered_rules: rulemap,
        apply: Box::new( move |c, r| {
            let (ref q0, ref word, ref so, ref q1, ref w) = *r;
            let (_, ref stack) = c.to;
            
            let s0 = stack.clone();
            let mut s1 = stack.clone();

            if let Some((s0, s1)) = match *so {
                KellerOp::Nothing => {
                    Some((s0, s1))
                },
                KellerOp::Add(ref e) => {
                    s1.insert(0, e.clone());
                    s1.truncate(depth);
                    Some((s0, s1))
                },
                KellerOp::Remove(ref e) => {
                    if s1.is_empty() {
                        Some((s0, s1))
                    } else if &(s1[0]) == e {
                        s1.remove(0);
                        Some((s0, s1))
                    } else { 
                        None
                    }
                },
                KellerOp::Replace(ref from, ref to) => {
                    if s1.is_empty() {
                        s1.insert(0, to.clone());
                        s1.truncate(depth);
                        Some((s0, s1))
                    } else if &(s1[0]) == from {
                        s1.remove(0);
                        s1.insert(0, to.clone());
                        s1.truncate(depth);
                        Some((s0, s1))
                    } else {
                        None
                    }
                }
            } {
                vec![
                    Arc{
                        from: (q0.clone(), s0),
                        to: (q1.clone(), s1),
                        label: word.clone(),
                        weight: *w
                    }
                ]
            } else {
                Vec::new()
            }
        } ),
        accepting: Box::new(| _ | true),
        already_found: BTreeSet::new(),
        item_map: Box::new(| x, _ | x)
    }
}

/// Creates Generator automata that are intersected with an approximation of the Dyck language of `Delta<T>`.
pub struct ApproxGeneratorAutomaton(pub usize);

impl GeneratorAutomaton for ApproxGeneratorAutomaton {
    fn convert<T, N>(&self, rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, initial: N) -> Automaton<BracketFragment<T>>
    where
        T: Clone + Hash + Eq + Ord,
        N: Clone + Hash + Eq + Ord
    {
        let (stack_transitions, start, stop) = mcfg_to_stack_transitions(rules, initial);
        let arcs: Vec<Arc<ApproxState<N>, BracketFragment<T>>> = stack_transitions_to_finite_transitions(stack_transitions, &start, self.0).collect();

        Automaton::from_arcs((start, vec![]), vec![(stop, vec![])], arcs)
    }
}

#[cfg(test)]
mod test {

    use std::fs::File;
    use std::io::Read;
    use pmcfg::PMCFG;
    use cs_representation::MCFG;
    use log_domain::LogDomain;
    use integeriser::{Integeriser, HashIntegeriser};
    use super::{mcfg_to_stack_transitions, stack_transitions_to_finite_transitions};
    use dyck::Bracket;

    #[test]
    fn approx() {
        let mut grammar_string = String::new();
        File::open("examples/example_mcfg.gr").unwrap().read_to_string(&mut grammar_string).expect("failed to read file");
        
        let pmcfg: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();
        let grammar: MCFG<String, String, LogDomain<f32>> = pmcfg.into();
        let initial = grammar.initial;
        let mut rules = HashIntegeriser::new();
        for rule in grammar.rules {
            rules.integerise(rule);
        }
        let (stack_approx, _, _) = mcfg_to_stack_transitions(&rules, initial);

        eprintln!("{:?}", stack_approx);

        for reg_trans in stack_transitions_to_finite_transitions(stack_approx, &Bracket::Open(("S".to_string(), 0)), 3) {
            eprintln!("{:?}", reg_trans);
        }
    }

}
