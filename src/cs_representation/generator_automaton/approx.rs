use openfsa::fsa::{Automaton, Arc};
use dyck::Bracket;
use cs_representation::{MCFG, BracketContent};
use pmcfg::{PMCFG, PMCFGRule, VarT};
use log_domain::LogDomain;
use integeriser::HashIntegeriser;
use integeriser::Integeriser;
use util::agenda::Agenda;
use std::collections::{BTreeSet, HashMap, BinaryHeap};
use std::hash::Hash;
use util::push_down::Pushdown;

pub type GeneratorAutomaton<T> = Automaton<Bracket<BracketContent<T>>>;

type StackElem = (usize, usize, usize);     // rule, component, index
type State<N> = Bracket<(N, usize)>;                 // nonterminal, component
type StackOp = (Option<StackElem>, Option<StackElem>);
type StackTransition<N, T> = (State<N>, Vec<Bracket<BracketContent<T>>>, StackOp, State<N>, LogDomain<f32>);

fn mcfg_to_stack_transitions<N, T>(
    rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>,
    initial: N
) -> (Vec<StackTransition<N, T>>, State<N>, State<N>)
where N: Clone + PartialEq + Hash + Eq,
      T: Clone + PartialEq + Hash + Eq
{
    let mut transitions = Vec::new();
    for rule_id in 0..rules.size() {
        let rule = rules.find_value(rule_id).unwrap();
        let weight_per_comp = rule.weight.pow( 1f32 / (rule.composition.composition.len() as f32) );
        for (component_id, component) in rule.composition.composition.iter().enumerate() {
            let weight_per_transition = weight_per_comp.pow( 1f32 / (component.iter().filter(|s| s.is_var()).count() + 1) as f32 );
            let mut terminals = vec![ Bracket::Open(BracketContent::Component(rule_id, component_id)) ];
            let mut stackop: Option<StackElem> = None;
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
                                terminals,
                                (stackop, Some((rule_id, component_id, symbol_id))),
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
                (q, terminals, (stackop, None), Bracket::Close((rule.head.clone(), component_id)), weight_per_transition)
            );
        }
    }

    (transitions, Bracket::Open((initial.clone(), 0)), Bracket::Close((initial, 0)))
}

/// Iterator for `recognise` that creates new solutions with every step
pub struct Recogniser<'a, A: Agenda<Item=(C, Pushdown<R>)>, C: Ord, R: Ord, K: Hash> {
    agenda: A,
    configuration_characteristic: Box<FnMut(&C) -> &K>,
    filtered_rules: HashMap<K, BinaryHeap<R>>,
    apply: Box<FnMut(&C, &R) -> Vec<C>>,
    accepting: Box<FnMut(&C) -> bool + 'a>,
    deduced: BTreeSet<C>,
}

impl<'a, A, C, R, K> Iterator for Recogniser<'a, A, C, R, K>
where A: Agenda<Item=(C, Pushdown<R>)>,
      C: Ord + Clone,
      R: Ord + Clone,
      K: Hash + Eq
{
    type Item = (C, Pushdown<R>);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((c, run)) = self.agenda.dequeue() {
            let deduced = &mut self.deduced;
            if let Some(rs) = self.filtered_rules.get((self.configuration_characteristic)(&c)) {
                for r in rs {
                    for c1 in (self.apply)(&c, r).into_iter().filter( |c1| deduced.insert(c1.clone()) ) {
                        let run1 = run.clone().push(r.clone());
                        self.agenda.enqueue((c1, run1));
                    }
                }
            }
            if (self.accepting)(&c) {
                return Some((c, run));
            }
        }

        None
    }
}

type StateStackTransition<N, T, S> = (N, Vec<T>, (Option<S>, Option<S>), N, LogDomain<f32>);
type ApproxStackTransitionIterator<N, T, S> = Recogniser<'static, Vec<(Arc<(N, Vec<S>), T>, Pushdown<StateStackTransition<N, T, S>>)>, Arc<(N, Vec<S>), T>, StateStackTransition<N, T, S>, N>;

fn stack_transitions_to_finite_transitions<N, T, S>(
    sa_transitions: Vec<StateStackTransition<N, T, S>>,
    start: N,
    depth: usize
    ) -> ApproxStackTransitionIterator<N, T, S>
where N: Clone + PartialEq + Hash + Eq + Ord,
      T: Clone + PartialEq + Ord,
      S: Clone + PartialEq + Ord
{
    let mut initial_agenda: Vec<(Arc<(N, Vec<S>), T>, Pushdown<StateStackTransition<N, T, S>>)> = Vec::new();
    for (q0, word, (s0, s), q1, w) in sa_transitions.clone() {
        if s0.is_none() && q0 == start {
            initial_agenda.push(
                (Arc::new((q0, vec![]), (q1, match s {None => vec![], Some(e) => vec![e]}), word, w), Pushdown::new())
            );
        }
    }
    let mut rulemap: HashMap<N, BinaryHeap<StateStackTransition<N, T, S>>> = HashMap::new();
    for trans in sa_transitions {
        let key = trans.0.clone();
        if rulemap.contains_key(&key) {
            let heap = rulemap.get_mut(&key).unwrap();
            heap.push(trans);
        } else {
            let mut heap = BinaryHeap::new();
            heap.push(trans);
            rulemap.insert(key, heap);
        }
    }

    Recogniser{
        agenda: initial_agenda,
        configuration_characteristic: Box::new( |c| {
            let (_, _, &(ref a, _), _) = c.untie();
            a
        }),
        filtered_rules: rulemap,
        apply: Box::new( move |c, r| {
            let &(ref q0, ref word, (ref e0, ref e1), ref q1, ref w) = r;
            let (_, _, &(_, ref stack), _) = c.untie();
            
            let mut s0 = stack.clone();
            let mut s1 = stack.clone();

            if s0.is_empty() {
                if let &Some(ref e) = e1 {
                    s1.insert(0, e.clone());
                    s1.truncate(depth);
                }
                vec![Arc::new((q0.clone(), s0), (q1.clone(), s1), word.clone(), w.clone())]
            } else {
                if let &Some(ref e) = e0 {
                    if e == &(s0[0]) {
                        s1.remove(0);
                        if let &Some(ref e_) = e1 {
                            s1.insert(0, e_.clone());
                        }
                        vec![Arc::new((q0.clone(), s0), (q1.clone(), s1), word.clone(), w.clone())]
                    } else {
                        Vec::new()
                    }
                } else {
                    if let &Some(ref e_) = e1 {
                        s1.insert(0, e_.clone());
                        s1.truncate(depth);
                    }
                    vec![Arc::new((q0.clone(), s0), (q1.clone(), s1), word.clone(), w.clone())]
                }
            }
        } ),
        accepting: Box::new(| _ | true),
        deduced: BTreeSet::new()
    }
}

pub fn generator<N, T>(grammar: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>, s: N, n: usize) -> GeneratorAutomaton<T>
where
    T: Clone + PartialEq + Hash + Eq + Ord,
    N: Clone + Hash + Eq + Ord
{
    let (stack_transitions, start, stop) = mcfg_to_stack_transitions(grammar, s);
    let arcs: Vec<Arc<(State<N>, Vec<StackElem>), Bracket<BracketContent<T>>>> = stack_transitions_to_finite_transitions(stack_transitions, start.clone(), n).map(|(c, _)| c).collect();

    Automaton::from_arcs(&(start, vec![]), vec![(stop, vec![])].as_slice(), arcs.as_slice())
}

#[test]
fn approx() {
    use std::fs::File;
    use std::io::Read;

    let mut grammar_string = String::new();
    File::open("examples/example_mcfg.gr").unwrap().read_to_string(&mut grammar_string);
    
    let pmcfg: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();
    let grammar: MCFG<String, String, LogDomain<f32>> = pmcfg.into();
    let initial = grammar.initial;
    let mut rules = HashIntegeriser::new();
    for rule in grammar.rules {
        rules.integerise(rule);
    }
    let (stack_approx, _, _) = mcfg_to_stack_transitions(&rules, initial);

    // println!("{:?}", stack_approx);

    for (reg_trans, _) in stack_transitions_to_finite_transitions(stack_approx, Bracket::Open(("S".to_string(), 0)), 3) {
        // println!("{:?}", reg_trans);
    }
}