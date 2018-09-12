/// This module implements the construction of finite automata for the regular
/// language in the C-S theorem for linear context-free rewriting systems and
/// the extraction of Dyck words from these constructed automata.

mod twin_state;
mod chart_entry;
mod rule_filter;
mod k_best;
mod heuristic;

pub use self::{ k_best::ChartIterator, rule_filter::{ CachedFilterPersistentStorage } };
use self::{ chart_entry::ChartEntry, twin_state::{TwinState, TwinRange, TwinArc}, heuristic::NaiveHeuristic };
use super::{BracketContent, rule_fragments::fragments};
use grammars::pmcfg::PMCFGRule;
use dyck::Bracket;
use util::{IntMap, agenda::{PriorityQueue, Capacity, Agenda}, search::WeightedSearchItem, vec_entry, factorizable::Factorizable, reverse::Reverse};

use integeriser::{HashIntegeriser, Integeriser};
use num_traits::{ One, Zero };
use std::{ collections::{ hash_map::Entry, BinaryHeap }, hash::Hash, ops::Mul, rc::Rc };
use fnv::FnvHashMap;

/// Represents a finite state automaton over a Dyck alphabet; optimized to extract
/// Dyck words from this automaton.
/// We consider this automaton as a construction from a grammar.
#[derive(Serialize, Deserialize, Debug)]
pub struct CykAutomatonPersistentStorage<T, W>
where
    T: Hash + Eq + Clone
{
    // Contains a list of `TwinArc`s for each grammar rule.
    // Each of those contains the state range, a terminal symbol and a weight
    // that correspond to a transition q → ⟨_a ⟩_a p in the automaton where a
    // is a terminal symbol in the grammar.
    // structure: rule id -> [ inital state, weight and symbol ]
    initials: Vec<Vec<TwinArc<W>>>,               // 
    // Contains a list of `TwinArc`s for each grammar rule.
    // structure: rule id -> [ TwinArc ]
    twin_arcs: Vec<Vec<(TwinState, TwinArc<W>)>>, 

    // The range between the initial and final states of the automaton.
    finals: TwinState,
    // Contains the terminal symbols.
    integeriser: Rc<HashIntegeriser<T>>
}

impl<T, W> CykAutomatonPersistentStorage<BracketContent<T>, W>
where
    T: Eq + Hash + Clone,
{
    /// Instantiates the `CykAutomatonPersistentStorage` for a specific word
    /// over terminal symbols of the grammar that was used to extract the
    /// automaton.
    /// Also, this constructor for `CykAutomaton` will apply a rule filter
    /// for rules of the grammar.
    pub fn intersect<I>(&self, rules: I, word: &[T]) -> CykAutomaton<BracketContent<T>, W>
    where
        T: Clone, W: Copy,
        I: IntoIterator<Item=usize>
    {
        let mut initial_map: IntMap<Vec<TwinArc<W>>> = IntMap::default();
        let mut twin_arcs: FnvHashMap<TwinState, Vec<TwinArc<W>>> = FnvHashMap::default();

        // only use initials and twin arcs for given rules
        for rule in rules {
            for tarc in self.initials.get(rule).into_iter().flat_map(|v| v) {
                initial_map.entry(tarc.label).or_insert_with(Vec::new).push(*tarc);
            }
            for &(from, to) in self.twin_arcs.get(rule).expect(&format!("rule {} was not initialized", rule)) {
                twin_arcs.entry(from).or_insert_with(Vec::new).push(to);
            }
        }

        // extract initial states from given twin arcs
        let mut initials = Vec::new();
        for (i, ti) in word.iter().cloned().enumerate() {
            if let Some(tid) = self.integeriser.find_key(&BracketContent::Terminal(ti)) {
                for ta in initial_map.get(&tid).into_iter().flat_map(|v| v) {
                    initials.push(
                        ( TwinRange { state: TwinState{ left: ta.left, right: ta.right }
                                    , range: TwinState{ left: i, right: i+1 }
                                    }
                        , tid
                        , ta.weight
                        )
                    );
                }
            } 
        }

        CykAutomaton {
            initials,
            twin_arcs,
            finals: TwinRange { state: self.finals, range: TwinState{ left: 0, right: word.len() } },
            
            integeriser: Rc::clone(&self.integeriser)
        }
    }
}

/// Represents an instantiation of a `CykAutomatonPersistentStorage` for a
/// specific word of the grammar that was used to extract the automaton.
pub struct CykAutomaton<T, W>
where
    T: Eq + Hash
{
    // Contains a list of initial state and index ranges together with
    // a terminal and the transition weight. 
    initials: Vec<(TwinRange, usize, W)>,
    // Pairs of transitions with matching parentheses.
    // structure:
    //    automata state range -> [ parenthesis, automata state range, weight ]
    twin_arcs: FnvHashMap<TwinState, Vec<TwinArc<W>>>,
    // Contains the target ranges, i.e. the initial and final state of the
    // finite state automaton and the first and last index of the word.
    finals: TwinRange,

    // Integerised terminals.
    integeriser: Rc<HashIntegeriser<T>>,
}

type StateT = usize;
type RangeT = usize;

impl<T, W> CykAutomaton<T, W>
where
    T: Hash + Eq,
    W: Ord + Copy + Mul<Output=W> + One + Zero + ::std::fmt::Debug + Factorizable
{
    pub fn fill_chart(&self) -> Chart<T, W> {
        let mut map: FnvHashMap<TwinRange, Vec<(ChartEntry<W>, W)>> = FnvHashMap::default();

        // initial items
        let mut agenda: BinaryHeap<(Reverse<_>, _, _)> = self.initials.iter().map(|&(range, label, weight)| (weight.into(), range, ChartEntry::Initial{ label, weight })).collect();

        // stores items via right (left) range component
        let mut to_right: FnvHashMap<(StateT, RangeT), Vec<(StateT, RangeT, Reverse<W>)>> = FnvHashMap::default();
        let mut to_left:  FnvHashMap<(StateT, RangeT), Vec<(StateT, RangeT, Reverse<W>)>> = FnvHashMap::default();

        while let Some((weight, tr, ce)) = agenda.pop() {
            match map.entry(tr) {
                // if the chart entry was already discovered, we don't need to
                // inviestigate any successors
                Entry::Occupied(mut entry) => {
                    match ce {
                        ChartEntry::Concat{ .. } => (),
                        _ => { entry.get_mut().push((ce, weight.unwrap())) }
                    }
                }
                Entry::Vacant(mut entry) => {
                    entry.insert(vec![(ce, weight.unwrap())]);
                    // keep the left (right) fringes of the current ranges for
                    // later concatenations
                    to_left.entry((tr.state.right, tr.range.right)).or_insert_with(Vec::new).push((tr.state.left, tr.range.left, weight));
                    to_right.entry((tr.state.left, tr.range.left)).or_insert_with(Vec::new).push((tr.state.right, tr.range.right, weight));

                    // apply all wrap operations, i.e. find applicable
                    // `TwinArcs` and enqueue the successors
                    for ta in self.twin_arcs.get(&tr.state).into_iter().flat_map(|v| v) {
                        agenda.push(
                            ( weight * ta.weight.into()
                            , tr.apply_state_arc(ta)
                            , ChartEntry::Wrap{ label: ta.label, inner: TwinState{ left: tr.state.left, right: tr.state.right }, weight: ta.weight }
                            ),
                        );
                    }

                    // investigate all concatenations to the left
                    for &(sleft, rleft, weight_) in to_left.get(&(tr.state.left, tr.range.left)).into_iter().flat_map(|v| v) {
                        agenda.push(
                            ( weight * weight_
                            , tr.expand_left(sleft, rleft)
                            , ChartEntry::Concat{ mid_state: tr.state.left, mid_range: tr.range.left }
                            ),
                        );
                    }

                    // and to the right
                    for &(sright, rright, weight_) in to_right.get(&(tr.state.right, tr.range.right)).into_iter().flat_map(|v| v) {
                        agenda.push(
                            ( weight * weight_
                            , tr.expand_right(sright, rright)
                            , ChartEntry::Concat{ mid_state: tr.state.right, mid_range: tr.range.right }
                            ),
                        );
                    }
                }
            }
            
        }

        Chart( map, self.finals, Rc::clone(&self.integeriser) )
    }
    /// Constructs a `Chart` from a `CykAutomaton`.
    /// This construction involves a Knuth search that is implemented using
    /// a priority Queue as an agenda structure. This Queue may be limited to
    /// hold only a specific amount of elements, i.e. this search implements
    /// ``beam seach''.
    pub fn fill_chart_beam(&self, beam: usize) -> Chart<T, W> {
        let heuristic = NaiveHeuristic::new(self);

        let mut map: FnvHashMap<TwinRange, Vec<(ChartEntry<W>, W)>> = FnvHashMap::default();
        let mut agenda = PriorityQueue::new(Capacity::Limit(beam));
        
        // stores items via right (left) range component
        let mut to_right: FnvHashMap<(StateT, RangeT), Vec<(StateT, RangeT, W)>> = FnvHashMap::default();
        let mut to_left:  FnvHashMap<(StateT, RangeT), Vec<(StateT, RangeT, W)>> = FnvHashMap::default();

        // enqueue initial items
        for &(range, label, weight) in &self.initials {
            if heuristic.wrap(range, weight) == W::zero() { continue; }
            agenda.enqueue(
                WeightedSearchItem((range, ChartEntry::Initial{ label, weight }, weight), heuristic.wrap(range, weight))
            );
        }

        while let Some(WeightedSearchItem((tr, ce, weight), _)) = agenda.dequeue() {
            match map.entry(tr) {
                // if the chart entry was already discovered, we don't need to
                // inviestigate any successors
                Entry::Occupied(mut entry) => {
                    match ce {
                        ChartEntry::Concat{ .. } => (),
                        _ => { entry.get_mut().push((ce, weight)) }
                    }
                }
                Entry::Vacant(mut entry) => {
                    entry.insert(vec![(ce, weight)]);
                    // keep the left (right) fringes of the current ranges for
                    // later concatenations
                    to_left.entry((tr.state.right, tr.range.right)).or_insert_with(Vec::new).push((tr.state.left, tr.range.left, weight));
                    to_right.entry((tr.state.left, tr.range.left)).or_insert_with(Vec::new).push((tr.state.right, tr.range.right, weight));

                    // apply all wrap operations, i.e. find applicable
                    // `TwinArcs` and enqueue the successors
                    for ta in self.twin_arcs.get(&tr.state).into_iter().flat_map(|v| v) {
                        if heuristic.wrap(tr.apply_state_arc(ta), weight * ta.weight) == W::zero() { continue; }
                        agenda.enqueue(
                            WeightedSearchItem(
                                ( tr.apply_state_arc(ta)
                                , ChartEntry::Wrap{ label: ta.label, inner: TwinState{ left: tr.state.left, right: tr.state.right }, weight: ta.weight }
                                , weight * ta.weight
                                ),
                                heuristic.wrap(tr.apply_state_arc(ta), weight * ta.weight)
                            )
                        );
                    }

                    // investigate all concatenations to the left
                    for &(sleft, rleft, weight_) in to_left.get(&(tr.state.left, tr.range.left)).into_iter().flat_map(|v| v) {
                        let successor = tr.expand_left(sleft, rleft);
                        if heuristic.wrap(successor, weight * weight_) == W::zero() { continue; }
                        agenda.enqueue(
                            WeightedSearchItem(
                                ( successor
                                , ChartEntry::Concat{ mid_state: tr.state.left, mid_range: tr.range.left }
                                , weight * weight_
                                ),
                                heuristic.wrap(successor, weight * weight_) 
                            )
                        );
                    }

                    // and to the right
                    for &(sright, rright, weight_) in to_right.get(&(tr.state.right, tr.range.right)).into_iter().flat_map(|v| v) {
                        let successor = tr.expand_right(sright, rright);
                        if heuristic.wrap(successor, weight * weight_) == W::zero() { continue; }
                        agenda.enqueue(
                            WeightedSearchItem(
                                ( successor
                                , ChartEntry::Concat{ mid_state: tr.state.right, mid_range: tr.range.right }
                                , weight * weight_
                                ),
                                heuristic.wrap(successor, weight * weight_) 
                            )
                        );
                    }
                }
            }
            
        }

        Chart( map, self.finals, Rc::clone(&self.integeriser) )
    }
}

/// A `Chart` contains a compact representation of all Dyck words that we
/// extracted from a `CykAutomaton`.
#[derive(Clone, Debug)]
pub struct Chart<T: Eq + Hash, W>(
    // Contains the actual chart, a relation between `TwinRange`s and
    // backtraces.
    FnvHashMap<TwinRange, Vec<(ChartEntry<W>, W)>>,
    // The root entry; it's the same as the initial/final state range of the
    // `CykAutomaton`. 
    TwinRange,

    // Contains the terminal symbols.
    Rc<HashIntegeriser<T>>
);

impl<T: Eq + Hash + Clone, W: Ord + Mul<Output=W> + Copy + One> IntoIterator for Chart<T, W> {
    type Item = Vec<Bracket<T>>;
    type IntoIter = ChartIterator<T, W>;
    fn into_iter(self) -> Self::IntoIter {
        ChartIterator::new(self)
    }
}

impl<T, W> CykAutomatonPersistentStorage<BracketContent<T>, W>
where
    T: Hash + Eq + Clone,
    W: Factorizable + Mul<Output=W> + One + Copy
{
    /// Extracts a `CykAutomatonPersistentStorage` from an `Lcfrs`.
    /// This involves the construction of the finite state automaton for the
    /// regular language in the C-S theorem for `Lcfrs` and a search for
    /// matching brackets to combine them for the `CykAutomatonPersistenStorage`
    /// in the following.
    /// TODO: this does not work for rules with epsilon rules
    pub fn from_grammar<'a, N, I>(rules: impl Iterator<Item=&'a PMCFGRule<N, T, W>>, integeriser: &I, init: N) -> Self
    where
        I: Integeriser<Item=PMCFGRule<N, T, W>>,
        N: Hash + Eq + Clone + 'a,
        T: 'a,
        W: 'a
    {
        // contains (p', t, weight, p, r) such that (p', ⟨_t, p) is an arc 
        // with weight w that was extracted from the rule r
        let mut opening = Vec::new();
        // t -> (q, q') such that (q, ⟩_t, q') is an arc
        let mut closing = IntMap::default();
        // contains (q, t, q', r) such that (q, ⟨_t ⟩_t, q') is an arc with 
        // weight w that was extracted from the rule r
        let mut initial = Vec::new();

        let mut states = HashIntegeriser::new();
        let mut terminals = HashIntegeriser::new();
        // the integeriser `states` will give positive integers,
        // we will fix unique states in the opposite direction.
        let mut uniquestate: i64 = -1;

        // stores the trainsition in one of the maps
        #[inline]
        fn store<TT: Eq + Hash + Clone, WT>((from, label, weight, to, rule): (usize, Bracket<TT>, WT, usize, usize),
                 terminals_: &mut HashIntegeriser<TT>,
                 open: &mut Vec<(usize, usize, WT, usize, usize)>,
                 close: &mut IntMap<(usize, usize)>) {
            match label {
                Bracket::Open(label) => open.push((from, terminals_.integerise(label), weight, to, rule)),
                Bracket::Close(label) => { close.insert(terminals_.integerise(label), (from, to)); }
            }
        }

        // constructs each transition and stores it
        for rule in rules {
            for fragment in fragments(&rule) {
                let rid = integeriser.find_key(&rule).unwrap();
                let ((q1, b1), ts, (qn, bn)) = fragment.singletons(integeriser);
                let weight = fragment.weight();
                store((states.integerise(q1), b1, weight, uniquestate as usize, rid), &mut terminals, &mut opening, &mut closing);
                store(((uniquestate - ts.len() as i64) as usize, bn, W::one(), states.integerise(qn), rid), &mut terminals, &mut opening, &mut closing);
                for (i, ti) in ts.iter().enumerate() {
                    initial.push(((uniquestate - i as i64) as usize, terminals.integerise(BracketContent::Terminal((*ti).clone())), (uniquestate - i as i64 - 1) as usize, rid));
                }
                uniquestate -= (ts.len() + 1) as i64;
            }
        }

        let mut initials: Vec<Vec<TwinArc<W>>> = Vec::new();
        let mut twin_arcs: Vec<Vec<(TwinState, TwinArc<W>)>> = Vec::new();

        // handles the initials
        for (left, label, right, rule) in initial {
            vec_entry(&mut initials, rule).push(TwinArc{ left, right, weight: W::one(), label });
        }

        // matches the transitions according to their terminal and cobines
        // them into `TwinArcs`
        for (left, label, weight, leftmid, rule) in opening {
            for &(rightmid, right) in closing.get(&label).into_iter() {
                vec_entry(&mut twin_arcs, rule).push(
                    ( TwinState{ left: leftmid, right: rightmid }
                    , TwinArc{ left, right, weight, label }
                    )
                );
            }
        }

        CykAutomatonPersistentStorage {
            initials,
            twin_arcs,
            finals: TwinState{ left: states.integerise(Bracket::Open((init.clone(), 0))), right: states.integerise(Bracket::Close((init, 0))) },
            integeriser: Rc::new(terminals)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use grammars::pmcfg::{Composition, VarT};
    use log_domain::LogDomain;

    fn example_grammar () -> Vec<PMCFGRule<&'static str, usize, LogDomain<f64>>> {
        let one = LogDomain::one();
        vec![
            PMCFGRule{ head: "S", tail: vec!["A"], weight: one, composition: Composition::from(vec![vec![VarT::Var(0, 1), VarT::Var(0, 0)]])},
            PMCFGRule{ head: "A", tail: vec!["A"], weight: one, composition: Composition::from(vec![vec![VarT::T(1), VarT::Var(0,0)], vec![VarT::Var(0, 1), VarT::T(2)]])},
            PMCFGRule{ head: "A", tail: vec!["B"], weight: one, composition: Composition::from(vec![vec![VarT::T(1), VarT::Var(0,0)], vec![VarT::Var(0, 1), VarT::T(2)]])},
            PMCFGRule{ head: "A", tail: vec![], weight: one, composition: Composition::from(vec![vec![VarT::T(0)], vec![VarT::T(0)]])},
            PMCFGRule{ head: "B", tail: vec![], weight: one, composition: Composition::from(vec![vec![VarT::T(3)], vec![VarT::T(3)]])}
        ]
    }
    
    #[test]
    fn construction_from_grammar () {
        let one = LogDomain::one();
        let rules = example_grammar();

        let mut r_integeriser = HashIntegeriser::new();
        for rule in &rules {
            r_integeriser.integerise(rule.clone());
        }

        let automaton = CykAutomatonPersistentStorage::from_grammar(rules.iter(), &r_integeriser, "S");
        let CykAutomatonPersistentStorage{ initials, twin_arcs, integeriser, finals } = automaton;

        let t0 = integeriser.find_key(&BracketContent::Terminal(0)).unwrap();
        let t1 = integeriser.find_key(&BracketContent::Terminal(1)).unwrap();
        let t2 = integeriser.find_key(&BracketContent::Terminal(2)).unwrap();
        let t3 = integeriser.find_key(&BracketContent::Terminal(3)).unwrap();

        assert_eq!(
            initials,
            vec![
                vec![],
                vec![ TwinArc{ left: (-4 as i64) as usize, right: (-5 as i64) as usize, label: t1, weight: one }
                    , TwinArc{ left: (-8 as i64) as usize, right: (-9 as i64) as usize, label: t2, weight: one }
                    ],
                vec![ TwinArc{ left: (-10 as i64) as usize, right: (-11 as i64) as usize, label: t1, weight: one }
                    , TwinArc{ left: (-14 as i64) as usize, right: (-15 as i64) as usize, label: t2, weight: one }
                    ],
                vec![ TwinArc{ left: (-16 as i64) as usize, right: (-17 as i64) as usize, label: t0, weight: one }
                    , TwinArc{ left: (-18 as i64) as usize, right: (-19 as i64) as usize, label: t0, weight: one }
                    ],
                vec![ TwinArc{ left: (-20 as i64) as usize, right: (-21 as i64) as usize, label: t3, weight: one }
                    , TwinArc{ left: (-22 as i64) as usize, right: (-23 as i64) as usize, label: t3, weight: one }
                    ],
            ]
        );

        assert_eq!(
            finals,
            TwinState{ left: 0, right: 5 }
        );

        let v00 = integeriser.find_key(&BracketContent::Variable(0, 0, 0)).unwrap();
        let v01 = integeriser.find_key(&BracketContent::Variable(0, 0, 1)).unwrap();
        let c0  = integeriser.find_key(&BracketContent::Component(0, 0)).unwrap();

        assert_eq!(
            twin_arcs[0],
            vec![
                (TwinState{ left: (-1 as i64) as usize, right: (-3 as i64) as usize }, TwinArc{ left: 0, right: 5, label: c0, weight: one }),
                (TwinState{ left: 1, right: 2 }, TwinArc{ left: (-1 as i64) as usize, right: (-2 as i64) as usize, label: v01, weight: one }),
                (TwinState{ left: 3, right: 4 }, TwinArc{ left: (-2 as i64) as usize, right: (-3 as i64) as usize, label: v00, weight: one }),
            ]
        );

        assert_eq!(twin_arcs.iter().flat_map(|v| v).count(), 15);
    }

    #[test]
    fn instantiation () {
        let one = LogDomain::one();
        let rules = example_grammar();
        let mut r_integeriser = HashIntegeriser::new();
        for rule in &rules {
            r_integeriser.integerise(rule.clone());
        }
        let automaton = CykAutomatonPersistentStorage::from_grammar(rules.iter(), &r_integeriser, "S").intersect(vec![0, 1, 2, 3, 4].into_iter(), &[2,2,3,1,1,3]);
        let CykAutomaton{ initials, twin_arcs, finals, integeriser } = automaton;

        assert_eq!(
            finals,
            TwinRange{ state: TwinState{ left: 0, right: 5 }, range: TwinState{ left: 0, right: 6 }}
        );

        let t1 = integeriser.find_key(&BracketContent::Terminal(1)).unwrap();
        let t2 = integeriser.find_key(&BracketContent::Terminal(2)).unwrap();
        let t3 = integeriser.find_key(&BracketContent::Terminal(3)).unwrap();

        assert_eq!(
            initials,
            vec![ (TwinRange{ state: TwinState{ left: (-8 as i64) as usize, right: (-9 as i64) as usize }, range: TwinState{ left: 0, right: 1 } }, t2, one)
                , (TwinRange{ state: TwinState{ left: (-14 as i64) as usize, right: (-15 as i64) as usize }, range: TwinState{ left: 0, right: 1 } }, t2, one)
                
                , (TwinRange{ state: TwinState{ left: (-8 as i64) as usize, right: (-9 as i64) as usize }, range: TwinState{ left: 1, right: 2 } }, t2, one)
                , (TwinRange{ state: TwinState{ left: (-14 as i64) as usize, right: (-15 as i64) as usize }, range: TwinState{ left: 1, right: 2 } }, t2, one)
                
                , (TwinRange{ state: TwinState{ left: (-20 as i64) as usize, right: (-21 as i64) as usize }, range: TwinState{ left: 2, right: 3 } }, t3, one)
                , (TwinRange{ state: TwinState{ left: (-22 as i64) as usize, right: (-23 as i64) as usize }, range: TwinState{ left: 2, right: 3 } }, t3, one)

                , (TwinRange{ state: TwinState{ left: (-4 as i64) as usize, right: (-5 as i64) as usize }, range: TwinState{ left: 3, right: 4 } }, t1, one)
                , (TwinRange{ state: TwinState{ left: (-10 as i64) as usize, right: (-11 as i64) as usize }, range: TwinState{ left: 3, right: 4 } }, t1, one)
                
                , (TwinRange{ state: TwinState{ left: (-4 as i64) as usize, right: (-5 as i64) as usize }, range: TwinState{ left: 4, right: 5 } }, t1, one)
                , (TwinRange{ state: TwinState{ left: (-10 as i64) as usize, right: (-11 as i64) as usize }, range: TwinState{ left: 4, right: 5 } }, t1, one)
                
                , (TwinRange{ state: TwinState{ left: (-20 as i64) as usize, right: (-21 as i64) as usize }, range: TwinState{ left: 5, right: 6 } }, t3, one)
                , (TwinRange{ state: TwinState{ left: (-22 as i64) as usize, right: (-23 as i64) as usize }, range: TwinState{ left: 5, right: 6 } }, t3, one)
            ]
        );

        assert_eq!(twin_arcs.values().flat_map(|v| v).count(), 15)
    }

    #[test]
    fn chart_construction () {
        use util::reverse::Reverse;
        let rules = example_grammar();
        let mut r_integeriser = HashIntegeriser::new();
        for rule in &rules {
            r_integeriser.integerise(rule.clone());
        }
        let initials: Vec<(TwinRange, usize, Reverse<LogDomain<f64>>)> = vec![(TwinRange{ state: TwinState{ left: 0, right: 1 }, range: TwinState{ left: 0, right: 1 } }, 0, LogDomain::one().into())];
        let twin_arcs = vec![
            (TwinState{ left: 0, right: 1 }, vec![TwinArc{ left: 2, right: 3, label: 1, weight: LogDomain::new(0.75).unwrap().into() }]),
            (TwinState{ left: 2, right: 3 }, vec![TwinArc{ left: 4, right: 5, label: 2, weight: LogDomain::one().into() }]),
            (TwinState{ left: 4, right: 5 }, vec![TwinArc{ left: 2, right: 3, label: 3, weight: LogDomain::new(0.25).unwrap().into() }]),
        ].into_iter().collect();
        let finals = TwinRange{ state: TwinState{ left: 2, right: 3 }, range: TwinState{ left: 0, right: 1 }};

        let automaton = CykAutomaton{ initials, twin_arcs, finals, integeriser: Rc::new(r_integeriser) };
        let Chart(map, _, _) = automaton.fill_chart();
        
        assert!(map.get(&finals).is_some());
    }
}