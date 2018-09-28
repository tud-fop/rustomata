/// This module implements a naive heuristic for the `CykAutomaton`.
/// It uses the transitions of the finite state automaton to compute the
/// optimal paths to the final and intial states.

use super::CykAutomaton;
use super::twin_state::{TwinRange, TwinState, StateArc};
use search::agenda::weighted::WeightedItem;
use util::{ factorizable::Factorizable };
use std::{ collections::{hash_map::Entry, BinaryHeap}, hash::Hash, ops::Mul };
use num_traits::{One, Zero};
use fnv::FnvHashMap;

type StateT = usize;
type RangeT = usize;

/// This structure contains the heuristic values for each state of the
/// `CykAutomaton` it was extracted from.
pub struct NaiveHeuristic<W> {
    // the weight of the optimal path from each state to the final state
    forward: FnvHashMap<(StateT, RangeT), W>,
    // the weight of the optimal path from the initial state to each state
    backward: FnvHashMap<(StateT, RangeT), W>
}

struct StateAutomaton<W> {
    transitions: FnvHashMap<StateT, Vec<(StateT, W)>>,
    transitions_with_range: FnvHashMap<(StateT, RangeT), Vec<(StateT, RangeT, W)>>,
    
    btransitions: FnvHashMap<StateT, Vec<(StateT, W)>>,
    btransitions_with_range: FnvHashMap<(StateT, RangeT), Vec<(StateT, RangeT, W)>>,
}

impl<W> StateAutomaton<W> 
where
    W: Copy + Factorizable
{
    /// Extracts the underlying transition system from a CykAutomaton.
    #[inline(always)]
    fn from_cyk_automaton<T: Eq + Hash>(automaton: &CykAutomaton<T, W>) -> StateAutomaton<W> {
        // separate `TwinArcs` and initials, since `TwinArcs` only affect the
        // state and initials also change the range
        let mut transitions = FnvHashMap::default();
        let mut transitions_with_range = FnvHashMap::default();
        
        let mut btransitions = FnvHashMap::default();
        let mut btransitions_with_range = FnvHashMap::default();

        // read off state behaviour of the finite state automaton
        for &(TwinRange{ state: TwinState{ left: fromst, right: tost }, range: TwinState{ left: fromr, right: tor } }, _, weight) in &automaton.initials {
            transitions_with_range.entry((fromst, fromr)).or_insert_with(Vec::new).push((tost, tor, weight));
            btransitions_with_range.entry((tost, tor)).or_insert_with(Vec::new).push((fromst, fromr, weight));
        }
        for (ts, vta) in &automaton.twin_arcs {
            for ta in vta {
                let (StateArc{ from, to, weight }, StateArc{ from: from_, to: to_, weight: weight_ }) = ta.split(ts);
                transitions.entry(from).or_insert_with(Vec::new).push((to, weight));
                transitions.entry(from_).or_insert_with(Vec::new).push((to_, weight_));
                btransitions.entry(to).or_insert_with(Vec::new).push((from, weight));
                btransitions.entry(to_).or_insert_with(Vec::new).push((from_, weight_));
            }
        }
        
        StateAutomaton{ transitions, btransitions, transitions_with_range, btransitions_with_range }
    }
}

impl<W> NaiveHeuristic<W>
where
    W: Mul<Output=W> + Copy + Ord + One + Factorizable
{
  
    /// Computes the `Heuristic` for a `CykAutomaton`.
    pub fn new<T: Eq + Hash>(automaton: &CykAutomaton<T, W>) -> Self
    where
        W: One
    {
        use self::Entry::*;

        let StateAutomaton{ 
            transitions, 
            btransitions, 
            transitions_with_range, 
            btransitions_with_range 
        } = StateAutomaton::from_cyk_automaton(automaton);

        let q0 = (automaton.finals.state.left, automaton.finals.range.left);
        let qf = (automaton.finals.state.right, automaton.finals.range.right);
        
        // forward pass:
        // start with initial state and search for optimal path to each other
        // state
        let mut agenda: BinaryHeap<WeightedItem<(StateT, RangeT), W>> = vec![WeightedItem(q0, W::one())].into_iter().collect();
        let mut backward = FnvHashMap::default();
        while let Some(WeightedItem(q, w)) = agenda.pop() {
            if let Vacant(ve) = backward.entry(q) {
                ve.insert(w);
                for &(to, weight) in transitions.get(&q.0).into_iter().flat_map(|v| v) {
                    agenda.push(WeightedItem((to, q.1), w * weight));
                }
                for &(tos, tor, weight) in transitions_with_range.get(&q).into_iter().flat_map(|v| v) {
                    agenda.push(WeightedItem((tos, tor), w * weight));
                }
            }
        }

        // backward pass starts with final state
        let mut agenda: BinaryHeap<WeightedItem<(StateT, RangeT), W>> = vec![WeightedItem(qf, W::one())].into_iter().collect();
        let mut forward = FnvHashMap::default();
        while let Some(WeightedItem(q, w)) = agenda.pop() {
            if let Vacant(ve) = forward.entry(q) {
                ve.insert(w);
                for &(to, weight) in btransitions.get(&q.0).into_iter().flat_map(|v| v) {
                    agenda.push(WeightedItem((to, q.1), w * weight));
                }
                for &(tos, tor, weight) in btransitions_with_range.get(&q).into_iter().flat_map(|v| v) {
                    agenda.push(WeightedItem((tos, tor), w * weight));
                }
            }
        }

        NaiveHeuristic{ forward, backward }
    }

    /// returns the product of a given weight and the heuristic for a givin
    /// `TwinRange`
    pub fn wrap(&self, state: TwinRange, weight: W) -> W
    where
        W: Zero
    {
        self.backward.get(&(state.state.left, state.range.left)).map(|p| *p).unwrap_or_else(W::zero)
            * weight
            * self.forward.get(&(state.state.right, state.range.right)).map(|p| *p).unwrap_or_else(W::zero)
    }
}

#[cfg(test)]
mod tests {
    use super::{*, super::twin_state::TwinArc};
    use integeriser::HashIntegeriser;
    use log_domain::LogDomain;
    use num_traits::{ One, Zero };
    use std::rc::Rc;
    use grammars::lcfrs::cs_representation::BracketContent;

    pub fn example_automaton() -> CykAutomaton<(), LogDomain<f64>> {
        let integeriser = HashIntegeriser::new();
        let initials = vec![(TwinRange{ state: TwinState{ left: 0, right: 1 }, range: TwinState{ left: 0, right: 1 } }, 0, LogDomain::one())];
        let twin_arcs = vec![
            (TwinState{ left: 0, right: 1 }, vec![TwinArc{ left: 2, right: 3, label: 1, weight: LogDomain::new(0.75).unwrap() }]),
            (TwinState{ left: 2, right: 3 }, vec![TwinArc{ left: 4, right: 5, label: 2, weight: LogDomain::one() }]),
            (TwinState{ left: 4, right: 5 }, vec![TwinArc{ left: 2, right: 3, label: 3, weight: LogDomain::new(0.25).unwrap() }]),
        ].into_iter().collect();
        let finals = TwinRange{ state: TwinState{ left: 2, right: 3 }, range: TwinState{ left: 0, right: 1 }};

        CykAutomaton{ initials, twin_arcs, finals, integeriser: Rc::new(integeriser) }
    }

    #[test]
    fn construction () {
        use util::factorizable::Factorizable;

        let automaton = example_automaton();
        let heuristic = NaiveHeuristic::new(&automaton);
        
        let one: LogDomain<f64> = LogDomain::one();
        let w1: LogDomain<f64> = LogDomain::new(0.25).unwrap();
        let w1 = w1.factorize(2)[0];
        let w2: LogDomain<f64> = LogDomain::new(0.75).unwrap();
        let w2 = w2.factorize(2)[0];

        assert_eq!(
            (0..=5).map(|q| heuristic.forward.get(&(q, 0))).collect::<Vec<_>>(),
            vec![Some(&w2), None, Some(&(w2 * w2)), None, Some(&(w2 * w2)), None]
        );
        assert_eq!(
            (0..=5).map(|q| heuristic.forward.get(&(q, 1))).collect::<Vec<_>>(),
            vec![None, Some(&w2), None, Some(&one), None, Some(&w1)]
        );
        assert_eq!(
            (0..=5).map(|q| heuristic.backward.get(&(q, 0))).collect::<Vec<_>>(),
            vec![Some(&w2), None, Some(&one), None, Some(&w1), None]
        );
        assert_eq!(
            (0..=5).map(|q| heuristic.backward.get(&(q, 1))).collect::<Vec<_>>(),
            vec![None, Some(&w2), None, Some(&(w2 * w2)), None, Some(&(w2 * w2))]
        );
    }

    #[test]
    fn wrap () {
        let automaton = example_automaton();
        let heuristic = NaiveHeuristic::new(&automaton);
        
        let zero: LogDomain<f64> = LogDomain::zero();
        let one: LogDomain<f64> = LogDomain::one();
        let w1: LogDomain<f64> = LogDomain::new(0.25).unwrap();
        let w2: LogDomain<f64> = LogDomain::new(0.75).unwrap();

        assert_eq!(
            vec![0, 2, 4].into_iter()
                         .zip(vec![1, 3, 5])
                         .map(|(left, right)| TwinRange{ state: TwinState{ left, right }, range: TwinState{ left: 0, right: 1 } })
                         .map(|tr| heuristic.wrap(tr, one))
                         .collect::<Vec<_>>(),
            vec![w2, one, w1]
        );

        assert_eq!(
            vec![0, 2, 4].into_iter()
                         .zip(vec![1, 3, 5])
                         .map(|(left, right)| TwinRange{ state: TwinState{ left, right }, range: TwinState{ left: 1, right: 1 } })
                         .map(|tr| heuristic.wrap(tr, one))
                         .collect::<Vec<_>>(),
            vec![zero, zero, zero]
        );

        assert_eq!(
            vec![0, 2, 4].into_iter()
                         .zip(vec![1, 3, 5])
                         .map(|(left, right)| TwinRange{ state: TwinState{ left, right }, range: TwinState{ left: 0, right: 0 } })
                         .map(|tr| heuristic.wrap(tr, one))
                         .collect::<Vec<_>>(),
            vec![zero, zero, zero]
        );
    }

    fn example_automaton2 () -> CykAutomaton<BracketContent<String>, LogDomain<f64>> {
        use super::super::CykAutomatonPersistentStorage;
        use grammars::lcfrs::Lcfrs;
        use integeriser::{HashIntegeriser, Integeriser};

        let Lcfrs{ rules, init }: Lcfrs<String, String, LogDomain<f64>>
                    = "initial: [S]\n\n
                       S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B) # 1\n
                       A → [[Var 0 0, Var 1 0], [Var 0 1, Var 2 0]] (A, W, X) # 0.4\n
                       A → [[Var 0 0], [Var 1 0]] (W, X) # 0.6\n
                       B → [[Var 0 0, Var 1 0], [Var 0 1, Var 2 0]] (B, Y, Z) # 0.3\n
                       B → [[Var 0 0], [Var 1 0]] (Y, Z) # 0.7\n
                       W → [[T a]] () # 1\n
                       X → [[T b]] () # 1\n
                       Y → [[T c]] () # 1\n
                       Z → [[T d]] () # 1".parse().unwrap();

        let mut int = HashIntegeriser::new();
        for rule in rules {
            int.integerise(rule);
        }
        
        let word: Vec<String> = vec!["a", "c", "b", "b", "d"].into_iter().map(|s| s.to_owned()).collect();
        CykAutomatonPersistentStorage::from_grammar(int.values().into_iter(), &int, init).intersect(0..=8, &word)
    }

    fn neg(n: usize) -> usize {
        -(n as i64) as usize
    }

    #[test]
    fn state_automaton () {
        use std::collections::BTreeSet;
        use util::factorizable::Factorizable;
        let automaton = example_automaton2();
        let state_automaton = StateAutomaton::from_cyk_automaton(&automaton);

        let one: LogDomain<f64> = LogDomain::one();
        
        let w1: LogDomain<f64> = LogDomain::new(0.4).unwrap();
        let w1 = w1.factorize(4)[0];
        let w2: LogDomain<f64> = LogDomain::new(0.6).unwrap();
        let w2 = w2.factorize(4)[0];
        let w3: LogDomain<f64> = LogDomain::new(0.3).unwrap();
        let w3 = w3.factorize(4)[0];
        let w4: LogDomain<f64> = LogDomain::new(0.7).unwrap();
        let w4 = w4.factorize(4)[0];

        assert_eq!(
            state_automaton.transitions_with_range,
            vec![
                ((neg(26),0), vec![(neg(27), 1, one)]),
                ((neg(30),1), vec![(neg(31), 2, one)]),
                ((neg(28),2), vec![(neg(29), 3, one)]),
                ((neg(28),3), vec![(neg(29), 4, one)]),
                ((neg(32),4), vec![(neg(33), 5, one)])
            ].into_iter().collect::<FnvHashMap<_, _>>()
        );

        assert_eq!(
            state_automaton.transitions.into_iter().flat_map(|(k, vs)| ::std::iter::repeat(k).zip(vs)).collect::<BTreeSet<_>>(),
            vec![
                // r1, S = 0/9, A-1 = 1/2, B-1 = 3/4, A-2 = 5/6, B-2 = 7/8
                (0, (neg(1), one)), (neg(1), (1, one)), (2, (neg(2), one)), (neg(2), (3, one)), (4, (neg(3), one)), 
                (neg(3), (5, one)), (6, (neg(4), one)), (neg(4), (7, one)), (8, (neg(5), one)), (neg(5), (9, one)),
                // r2, W = 10/11, X = 12/13
                (1, (neg(6), w1)), (neg(6), (1, one)), (2, (neg(7), one)), (neg(7), (10, one)), (11, (neg(8), one)), (neg(8), (2, w1)),
                (5, (neg(9), w1)), (neg(9), (5, one)), (6, (neg(10), one)), (neg(10), (12, one)), (13, (neg(11), one)), (neg(11), (6, w1)),
                // r3
                (1, (neg(12), w2)), (neg(12), (10, one)), (11, (neg(13), one)), (neg(13), (2, w2)),
                (5, (neg(14), w2)), (neg(14), (12, one)), (13, (neg(15), one)), (neg(15), (6, w2)),
                // r4, Y = 14/15, Z = 16/17
                (3, (neg(16), w3)), (neg(16), (3, one)), (4, (neg(17), one)), (neg(17), (14, one)), (15, (neg(18), one)), (neg(18), (4, w3)),
                (7, (neg(19), w3)), (neg(19), (7, one)), (8, (neg(20), one)), (neg(20), (16, one)), (17, (neg(21), one)), (neg(21), (8, w3)),
                // r5
                (3, (neg(22), w4)), (neg(22), (14, one)), (15, (neg(23), one)), (neg(23), (4, w4)),
                (7, (neg(24), w4)), (neg(24), (16, one)), (17, (neg(25), one)), (neg(25), (8, w4)),
                // r6, r7, r8, r9
                (10, (neg(26), one)), (neg(27), (11, one)),
                (12, (neg(28), one)), (neg(29), (13, one)),
                (14, (neg(30), one)), (neg(31), (15, one)),
                (16, (neg(32), one)), (neg(33), (17, one)),
            ].into_iter().collect::<BTreeSet<_>>()
        );
    }

    #[test]
    fn construction2 () {
        use util::factorizable::Factorizable;
        let automaton = example_automaton2();
        let heuristic = NaiveHeuristic::new(&automaton);
        let one: LogDomain<f64> = LogDomain::one();
    
        let w2: LogDomain<f64> = LogDomain::new(0.6).unwrap();
        let w2 = w2.factorize(4)[0];
        let w4: LogDomain<f64> = LogDomain::new(0.7).unwrap();
        let w4 = w4.factorize(4)[0];

        assert_eq!(
            vec![(0, 0), (neg(1), 0), (1, 0), (2, 1), (4, 2)].into_iter().map(|s| heuristic.backward.get(&s)).collect::<Vec<_>>(),
            vec![Some(&one), Some(&one), Some(&one), Some(&(w2 * w2)), Some(&(w2 * w2 * w4 * w4))],
        );
        assert_eq!(
            vec![
                (0, 0), (neg(1), 0), (1, 0),
                (2, 1), (4, 2), (6, 4), (8, 5)
            ].into_iter().map(|s| heuristic.forward.get(&s)).collect::<Vec<_>>(),
            vec![
                Some(&(w2 * w2 * w4 * w4 * w2 * w2 * w2 * w4 * w4)), Some(&(w2 * w2 * w4 * w4 * w2 * w2 * w2 * w4 * w4)), Some(&(w2 * w2 * w4 * w4 * w2 * w2 * w2 * w4 * w4)),
                Some(&(w4 * w4 * w2 * w2 * w2 * w4 * w4)), Some(&(w2 * w2 * w2 * w4 * w4)), Some(&(w4 * w4)), Some(&one)
            ],
        );
        assert_eq!(
            heuristic.forward.get(&(9,5)),
            Some(&one),
        );
    }
}