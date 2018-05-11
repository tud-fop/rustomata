/// extraction of Dyck words from an FSA over bracket words similar as described by Hulden, 2011
/// TODO: * this should not need terminal-separated rules

use dyck::Bracket;
use grammars::lcfrs::cs_representation::{automata::FiniteAutomaton,
                                         bracket_fragment::BracketFragment, BracketContent};
use integeriser::{Integeriser, HashIntegeriser};
use num_traits::{One, Zero};
use std::ops::Mul;
use util::{search::{Search, WeightedSearchItem},
           agenda::Capacity};

use std::{collections::HashMap, hash::Hash, cmp::max};

mod iterator;
mod chart_entry;

#[derive(Debug, PartialEq, Eq)]
struct CykGeneratorHeuristic<W> {
    forward: HashMap<u32, W>,
    backward: HashMap<u32, W>,
}

impl<W> CykGeneratorHeuristic<W> {
    fn new<C: Hash + Eq>(fsa: &ExplodedAutomaton<C, W>) -> Self
    where
        W: One + Mul<Output = W> + Copy + Ord,
    {
        let forward = Search::weighted(
            fsa.finals.iter().map(|q| WeightedSearchItem(*q, W::one())),
            |&WeightedSearchItem(q, w)| {
                fsa.backward_state_transition
                    .get(&q)
                    .into_iter()
                    .flat_map(|v| v.iter().map(|&(q_, w_)| WeightedSearchItem(q_, w_ * w)))
                    .collect()
            },
        ).uniques()
            .map(|WeightedSearchItem(q, w)| (q, w))
            .collect();

        let backward = Search::weighted(
            vec![WeightedSearchItem(fsa.initial, W::one())],
            |&WeightedSearchItem(q, w)| {
                fsa.forward_state_transition
                    .get(&q)
                    .into_iter()
                    .flat_map(|v| v.iter().map(|&(q_, w_)| WeightedSearchItem(q_, w * w_)))
                    .collect()
            },
        ).uniques()
            .map(|WeightedSearchItem(q, w)| (q, w))
            .collect();

        CykGeneratorHeuristic { forward, backward }
    }

    fn wrap(&self, p: &u32, w: W, q: &u32) -> W
    where
        W: Zero + Mul<Output = W> + Copy,
    {
        self.backward.get(p).map(|p| *p).unwrap_or(W::zero()) * w
            * self.forward.get(q).map(|p| *p).unwrap_or(W::zero())
    }
}

#[derive(Clone)]
pub struct ExplodedAutomaton<T, W>
where
    T: Eq + Hash,
{
    initial: u32,
    finals: Vec<u32>,

    forward_state_transition: HashMap<u32, Vec<(u32, W)>>,
    backward_state_transition: HashMap<u32, Vec<(u32, W)>>,

    terminal_i: HashIntegeriser<Vec<Bracket<T>>>,
    bracket_i: HashIntegeriser<T>,

    terminal: Vec<(u32, u8, W, u32)>,
    opening_brackets: Vec<(u32, u32, W, u32)>,
    closing_brackets: HashMap<u32, Vec<(u32, W, u32)>>,
}

impl<T, W> ::std::fmt::Debug for ExplodedAutomaton<T, W>
where
    T: ::std::fmt::Debug + Hash + Eq + Clone,
    W: ::std::fmt::Debug + Copy
{
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        for &(q, ts, w, q_) in &self.terminal {
            writeln!(f, "{:?}: {} → {} # {:?}", self.terminal_i.find_value(ts as usize).unwrap(), q, q_, w)?;
        }
        for &(q1, cont, w1, q1_) in &self.opening_brackets {
            for &(q2, w2, q2_) in self.closing_brackets.get(&cont).unwrap_or(&Vec::new()) {
                writeln!(f, "{:?}: {} → {} # {:?}, {} → {} # {:?}", self.bracket_i.find_value(cont as usize).unwrap(), q1, q1_, w1, q2, q2_, w2)?;
            }
        }
        Ok(())
    }
}

impl<W, T> ExplodedAutomaton<BracketContent<T>, W>
where
    W: Copy + One,
    T: Hash + Eq + Clone,
{
    fn new(fsa: FiniteAutomaton<BracketFragment<T>, W>) -> Self {
        // contains (p', ⟨_t, p) such that (p', t, weight, p)
        let mut opening = Vec::new();
        // contains (q, ⟩_t, q') such that t -> (q, weight, q')
        let mut closing = HashMap::new();
        // forward state transitions without labels
        let mut forward = HashMap::new();
        // backward state transitions without labels
        let mut backward = HashMap::new();
        // all bracket fragments with terminal symbols
        let mut initial = Vec::new();

        let mut terminal_i = HashIntegeriser::new();
        let mut bracket_i = HashIntegeriser::new();

        // counter for unique usize states while exploding fsa transitions
        let mut uniquestate = max(fsa.arcs.len(), fsa.arcs.iter().flat_map(|m| m.values().map(|&(q, _)| q)).max().unwrap_or(0) + 1) as u32;

        for (from, tos) in fsa.arcs.into_iter().enumerate() {
            for (ilabel, (to, weight)) in tos.into_iter() {
                let BracketFragment(mut brackets) = fsa.labels.find_value(ilabel).unwrap().clone();
                if brackets.len() == 2 {
                    let bracket = brackets.remove(0);
                    match bracket {
                        Bracket::Open(cont) => {
                            opening.push((from as u32, bracket_i.integerise(cont) as u32, weight, uniquestate));
                            forward
                                .entry(from as u32)
                                .or_insert_with(Vec::new)
                                .push((uniquestate, weight));
                            backward
                                .entry(uniquestate)
                                .or_insert_with(Vec::new)
                                .push((from as u32, weight));
                        }
                        Bracket::Close(cont) => {
                            closing.entry(bracket_i.integerise(cont) as u32).or_insert_with(Vec::new).push((
                                from as u32,
                                W::one(),
                                uniquestate,
                            ));
                            forward
                                .entry(from as u32)
                                .or_insert_with(Vec::new)
                                .push((uniquestate, W::one()));
                            backward
                                .entry(uniquestate)
                                .or_insert_with(Vec::new)
                                .push((from as u32, W::one()));
                        }
                    }

                    let bracket = brackets.remove(0);
                    match bracket {
                        Bracket::Open(cont) => {
                            opening.push((uniquestate, bracket_i.integerise(cont) as u32, W::one(), to as u32));
                            forward
                                .entry(uniquestate)
                                .or_insert_with(Vec::new)
                                .push((to as u32, W::one()));
                            backward
                                .entry(to as u32)
                                .or_insert_with(Vec::new)
                                .push((uniquestate, W::one()));
                        }
                        Bracket::Close(cont) => {
                            closing.entry(bracket_i.integerise(cont) as u32).or_insert_with(Vec::new).push((
                                uniquestate,
                                weight,
                                to as u32,
                            ));
                            forward
                                .entry(uniquestate)
                                .or_insert_with(Vec::new)
                                .push((to as u32, weight));
                            backward
                                .entry(to as u32)
                                .or_insert_with(Vec::new)
                                .push((uniquestate, weight));
                        }
                    }

                    uniquestate += 1;
                } else if brackets.len() == 4 {
                    initial.push((from as u32, terminal_i.integerise(brackets) as u8, weight, to as u32));
                    forward
                        .entry(from as u32)
                        .or_insert_with(Vec::new)
                        .push((to as u32, weight));
                    backward
                        .entry(to as u32)
                        .or_insert_with(Vec::new)
                        .push((from as u32, weight));
                } else {
                    panic!("encountered `BracketFragment` with length other than 2 and 4");
                }
            }
        }

        ExplodedAutomaton {
            initial: fsa.initial as u32,
            finals: fsa.finals.into_iter().map(|q| q as u32).collect(),

            forward_state_transition: forward,
            backward_state_transition: backward,

            terminal_i,
            bracket_i,

            terminal: initial,
            opening_brackets: opening,
            closing_brackets: closing,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
enum ChartEntry<W> {
    Initial(u8, W),
    Concat(u32, u32, u32),
    Bracketed(u32, W, u32, u32, W)
}

#[derive(Debug, Clone)]
pub struct GenerationChart<W>(
    HashMap<(u32, u32), Vec<(ChartEntry<W>, W)>>
);

impl<W> GenerationChart<W> {
    pub fn fill<T>(fsa: &ExplodedAutomaton<T, W>, beam: Capacity) -> Self
    where
        T: Clone + Hash + Eq + Ord + ::std::fmt::Debug,
        W: Mul<Output = W> + Zero + One + Ord + Copy + ::std::fmt::Debug,
    {
        use util::agenda::{Agenda, PriorityQueue};
        use self::ChartEntry::*;
        use std::collections::hash_map::Entry;

        let h = CykGeneratorHeuristic::new(&fsa);
        let ExplodedAutomaton {
            terminal,
            opening_brackets,
            closing_brackets,
            ..
        } = fsa;

        let mut bpairs = HashMap::new();
        for &(from1, cont, w1, to1) in opening_brackets {
            for &(from2, w2, to2) in closing_brackets.get(&cont).into_iter().flat_map(|v| v) {
                bpairs.entry((to1, from2)).or_insert_with(Vec::new).push((
                    from1,
                    cont,
                    w1,
                    w2,
                    to2,
                ));
            }
        }

        let mut chart: HashMap<(u32, u32), Vec<(ChartEntry<W>, W)>> = HashMap::new();
        let mut from_left: HashMap<u32, HashMap<u32, W>> = HashMap::new();
        let mut from_right: HashMap<u32, HashMap<u32, W>> = HashMap::new();

        let mut agenda: PriorityQueue<_, _> = terminal.into_iter().map(
            |&(p, t, w, q)| WeightedSearchItem((p, w, q, Initial(t, w)), h.wrap(&p, w, &q))
        ).collect();
        
        if let Capacity::Limit(c) = beam {
            agenda.set_capacity(c);
        }

        while let Some(WeightedSearchItem((p, w, q, t), _)) = agenda.dequeue() {
            match chart.entry((p, q)) {
                Entry::Occupied(mut oe) => {
                    match t {
                        Initial(_, _) | Bracketed(_, _, _, _, _) => { oe.get_mut().push((t, w)); },
                        Concat(_, _, _) => ()
                    }
                    
                },
                Entry::Vacant(mut ve) => {
                    ve.insert(vec![(t, w)]);
                    from_left.entry(p).or_insert_with(HashMap::new).entry(q).or_insert(w);
                    from_right.entry(q).or_insert_with(HashMap::new).entry(p).or_insert(w);
                    
                    for (q_, w_) in from_left.get(&q).into_iter().flat_map(|m| m) {
                        let weight = w * *w_;
                        let priority = h.wrap(&p, weight, q_);
                        if !priority.is_zero() {
                            agenda.enqueue(WeightedSearchItem((p, weight, *q_, Concat(p, q, *q_)), priority));
                        }
                    }
                    for (p_, w_) in from_right.get(&p).into_iter().flat_map(|m| m) {
                        let weight = *w_ * w;
                        let priority = h.wrap(p_, weight, &q);
                        if !priority.is_zero() {
                            agenda.enqueue(WeightedSearchItem((*p_, weight, q, Concat(*p_, p, q)), priority));
                        }
                    }
                    for &(p_, cont, w1, w2, q_) in bpairs.get(&(p, q)).into_iter().flat_map(|v| v) {
                        let weight = w1 * w * w2;
                        let priority = h.wrap(&p_, weight, &q_);
                        if !priority.is_zero() {
                            agenda.enqueue(WeightedSearchItem((p_, weight, q_, Bracketed(cont, w1, p, q, w2)), priority));
                        }
                    }

                }
            }
        }
        
        GenerationChart(chart)
    }
}

pub fn cyk_generator<T, W>(fsa: FiniteAutomaton<BracketFragment<T>, W>, beam: Capacity) -> impl Iterator<Item=Vec<Bracket<BracketContent<T>>>>
where
    T: Clone + Hash + Eq + Ord + ::std::fmt::Debug,
    W: Mul<Output = W> + Zero + One + Ord + Copy + ::std::fmt::Debug,
{
    let exploded = ExplodedAutomaton::new(fsa);
    let chart = GenerationChart::fill(&exploded, beam);

    iterator::ChartIterator::new(chart, exploded)
}



#[cfg(test)]
mod tests {
    use super::*;
    use grammars::lcfrs::cs_representation::{automata::finite_automaton::StateInstruction,
                                             BracketContent};
    use log_domain::LogDomain;
    use num_traits::One;
    use recognisable::Transition;
    use util::reverse::Reverse;

    #[test]
    fn test_chart() {
        let one = LogDomain::one().into();
        let w1 = LogDomain::new(0.5).unwrap().into();
        let w2 = LogDomain::new(0.75).unwrap().into();
        
        use self::ChartEntry::{Initial, Bracketed};
        let exploded = ExplodedAutomaton::new(example_fsa());
        let chart = GenerationChart::fill(&exploded, Capacity::Infinite);

        let terminal = exploded.terminal_i.find_key(
                    &vec![
                        Bracket::Open(BracketContent::Component(1, 0)),
                        Bracket::Open(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Component(1, 0)),
                    ]
        ).unwrap() as u8;

        assert_eq!(
            chart.0,
            vec![
                ((0, 1), vec![
                    (Initial(terminal, w2), w2),
                    (Bracketed(exploded.bracket_i.find_key(&BracketContent::Component(0, 0)).unwrap() as u32, w1, 2, 3, w1), w1 * w2 * w1)
                ]),
                ((2, 3), vec![
                    (Bracketed(exploded.bracket_i.find_key(&BracketContent::Variable(0, 0, 0)).unwrap() as u32, one, 0, 1, one),  w2)
                ])
            ].into_iter().collect()
        );
    }
    
    #[test]
    fn test_explode() {
        let one = LogDomain::one().into();
        let w1 = LogDomain::new(0.5).unwrap().into();
        let w2 = LogDomain::new(0.75).unwrap().into();
        
        let exploded = ExplodedAutomaton::new(example_fsa());

        assert_eq!(exploded.initial, 0);
        assert_eq!(exploded.finals, vec![1]);
        assert_eq!(
            exploded.forward_state_transition,
            vec![
                (0, vec![(1, w2), (2, w1)]),
                (2, vec![(0, one)]),
                (1, vec![(3, one)]),
                (3, vec![(1, w1)]),
            ].into_iter()
                .collect()
        );
        assert_eq!(
            exploded.backward_state_transition,
            vec![
                (0, vec![(2, one)]),
                (2, vec![(0, w1)]),
                (1, vec![(0, w2), (3, w1)]),
                (3, vec![(1, one)]),
            ].into_iter()
                .collect(),
        );
        assert_eq!(
            exploded.terminal,
            vec![(
                0,
                exploded.terminal_i.find_key(
                    &vec![
                        Bracket::Open(BracketContent::Component(1, 0)),
                        Bracket::Open(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Component(1, 0)),
                    ]
                ).unwrap() as u8,
                w2,
                1,
            )]
        );
        assert_eq!(
            exploded.opening_brackets,
            vec![
                (0, exploded.bracket_i.find_key(&BracketContent::Component(0, 0)).unwrap() as u32, w1, 2),
                (2, exploded.bracket_i.find_key(&BracketContent::Variable(0, 0, 0)).unwrap() as u32, one, 0),
            ]
        );
        assert_eq!(
            exploded.closing_brackets,
            vec![
                (exploded.bracket_i.find_key(&BracketContent::Variable(0, 0, 0)).unwrap() as u32, vec![(1, one, 3)]),
                (exploded.bracket_i.find_key(&BracketContent::Component(0, 0)).unwrap() as u32, vec![(3, w1, 1)]),
            ].into_iter()
                .collect()
        )
    }

    #[test]
    fn test_cyk_heuristic() {
        let one = LogDomain::one().into();
        let w1 = LogDomain::new(0.5).unwrap().into();
        let w2 = LogDomain::new(0.75).unwrap().into();
        
        let h = CykGeneratorHeuristic::new(&ExplodedAutomaton::new(example_fsa()));

        assert_eq!(
            CykGeneratorHeuristic {
                forward: 
                    vec![
                        (0, w2),
                        (1, one),
                        (2, w2),
                        (3, w1)
                    ].into_iter()
                    .collect(),
                backward: 
                    vec![
                        (0, one), 
                        (1, w2), 
                        (2, w1), 
                        (3, w2)
                    ].into_iter()
                    .collect(),
            },
            h
        );

        let v = LogDomain::new(0.5).unwrap().into();
        assert_eq!(v, h.wrap(&0, v, &1));
    }

    pub fn example_fsa() -> FiniteAutomaton<BracketFragment<String>, Reverse<LogDomain<f64>>> {
        let arcs = vec![
            Transition {
                weight: LogDomain::new(0.5).unwrap().into(),
                instruction: StateInstruction(0, 0),
                word: vec![BracketFragment(vec![
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                ])],
            },
            Transition {
                weight: LogDomain::new(0.75).unwrap().into(),
                instruction: StateInstruction(0, 1),
                word: vec![BracketFragment(vec![
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Component(1, 0)),
                ])],
            },
            Transition {
                weight: LogDomain::new(0.5).unwrap().into(),
                instruction: StateInstruction(1, 1),
                word: vec![BracketFragment(vec![
                    Bracket::Close(BracketContent::Variable(0, 0, 0)),
                    Bracket::Close(BracketContent::Component(0, 0)),
                ])],
            },
        ];

        FiniteAutomaton::new(arcs, 0, vec![1])
    }
}
