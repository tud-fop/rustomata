/// This module implements a naive heuristic for the `CykAutomaton`.
/// It uses the transitions of the finite state automaton to compute the
/// optimal paths to the final and intial states.

use super::CykAutomaton;
use super::twin_state::{TwinRange, TwinState, StateArc};
use util::{ agenda::{PriorityQueue, Agenda}, search::WeightedSearchItem };
use std::{ collections::{HashMap, hash_map::Entry}, hash::Hash, ops::Mul };
use num_traits::{One, Zero};

type StateT = usize;
type RangeT = usize;

/// This structure contains the heuristic values for each state of the
/// `CykAutomaton` it was extracted from.
pub struct NaiveHeuristic<W> {
    // the weight of the optimal path from each state to the final state
    forward: HashMap<(StateT, RangeT), W>,
    // the weight of the optimal path from the initial state to each state
    backward: HashMap<(StateT, RangeT), W>
}

impl<W> NaiveHeuristic<W>
where
    W: Mul<Output=W> + Copy + Ord
{
    /// Computes the `Heuristic` for a `CykAutomaton`.
    pub fn new<T: Eq + Hash>(automaton: &CykAutomaton<T, W>) -> Self
    where
        W: One
    {
        use self::Entry::*;

        // separate `TwinArcs` and initials, since `TwinArcs` only affect the
        // state and initials also change the range
        let mut state_graph = HashMap::new();
        let mut state_graph_with_range = HashMap::new();
        
        let mut backward_state_graph = HashMap::new();
        let mut backward_state_graph_with_range = HashMap::new();

        // read off state behaviour of the finite state automaton
        for &(TwinRange{ state: TwinState{ left: fromst, right: tost }, range: TwinState{ left: fromr, right: tor } }, _, weight) in &automaton.initials {
            state_graph_with_range.entry((fromst, fromr)).or_insert_with(Vec::new).push((tost, tor, weight));
            backward_state_graph_with_range.entry((tost, tor)).or_insert_with(Vec::new).push((fromst, fromr, weight));
        }
        for (ts, vta) in &automaton.twin_arcs {
            for ta in vta {
                let (StateArc{ from, to, weight }, StateArc{ from: from_, to: to_, weight: weight_ }) = ta.split(ts);
                state_graph.entry(from).or_insert_with(Vec::new).push((to, weight));
                state_graph.entry(from_).or_insert_with(Vec::new).push((to_, weight_));
                backward_state_graph.entry(to).or_insert_with(Vec::new).push((from, weight));
                backward_state_graph.entry(to_).or_insert_with(Vec::new).push((from_, weight_));
            }
        }

        let q0 = (automaton.finals.state.left, automaton.finals.range.left);
        let qf = (automaton.finals.state.right, automaton.finals.range.right);
        
        // forward pass:
        // start with initial state and search for optimal path to each other
        // state
        let mut agenda: PriorityQueue<W, WeightedSearchItem<(StateT, RangeT), W>> = vec![WeightedSearchItem(q0, W::one())].into_iter().collect();
        let mut backward = HashMap::new();
        while let Some(WeightedSearchItem(q, w)) = agenda.dequeue() {
            if let Vacant(ve) = backward.entry(q) {
                ve.insert(w);
                for &(to, weight) in state_graph.get(&q.0).into_iter().flat_map(|v| v) {
                    agenda.enqueue(WeightedSearchItem((to, q.1), w * weight));
                }
                for &(tos, tor, weight) in state_graph_with_range.get(&q).into_iter().flat_map(|v| v) {
                    agenda.enqueue(WeightedSearchItem((tos, tor), w * weight));
                }
            }
        }

        // backward pass starts with final state
        let mut agenda: PriorityQueue<W, WeightedSearchItem<(StateT, RangeT), W>> = vec![WeightedSearchItem(qf, W::one())].into_iter().collect();
        let mut forward = HashMap::new();
        while let Some(WeightedSearchItem(q, w)) = agenda.dequeue() {
            if let Vacant(ve) = forward.entry(q) {
                ve.insert(w);
                for &(to, weight) in backward_state_graph.get(&q.0).into_iter().flat_map(|v| v) {
                    agenda.enqueue(WeightedSearchItem((to, q.1), w * weight));
                }
                for &(tos, tor, weight) in backward_state_graph_with_range.get(&q).into_iter().flat_map(|v| v) {
                    agenda.enqueue(WeightedSearchItem((tos, tor), w * weight));
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
    use util::reverse::Reverse;

    pub fn example_automaton() -> CykAutomaton<(), Reverse<LogDomain<f64>>> {
        let integeriser = HashIntegeriser::new();
        let initials = vec![(TwinRange{ state: TwinState{ left: 0, right: 1 }, range: TwinState{ left: 0, right: 1 } }, 0, LogDomain::one().into())];
        let twin_arcs = vec![
            (TwinState{ left: 0, right: 1 }, vec![TwinArc{ left: 2, right: 3, label: 1, weight: LogDomain::new(0.75).unwrap().into() }]),
            (TwinState{ left: 2, right: 3 }, vec![TwinArc{ left: 4, right: 5, label: 2, weight: LogDomain::one().into() }]),
            (TwinState{ left: 4, right: 5 }, vec![TwinArc{ left: 2, right: 3, label: 3, weight: LogDomain::new(0.25).unwrap().into() }]),
        ].into_iter().collect();
        let finals = TwinRange{ state: TwinState{ left: 2, right: 3 }, range: TwinState{ left: 0, right: 1 }};

        CykAutomaton{ initials, twin_arcs, finals, integeriser: Rc::new(integeriser) }
    }

    #[test]
    fn construction () {
        let automaton = example_automaton();
        let heuristic = NaiveHeuristic::new(&automaton);
        let one: Reverse<LogDomain<f64>> = LogDomain::one().into();
        let w1: Reverse<LogDomain<f64>> = LogDomain::new(0.25).unwrap().into();
        let w2: Reverse<LogDomain<f64>> = LogDomain::new(0.75).unwrap().into();

        assert_eq!(
            (0..=5).map(|q| heuristic.forward.get(&(q, 0))).collect::<Vec<_>>(),
            vec![Some(&one), None, Some(&w2), None, Some(&w2), None]
        );
        assert_eq!(
            (0..=5).map(|q| heuristic.forward.get(&(q, 1))).collect::<Vec<_>>(),
            vec![None, Some(&one), None, Some(&one), None, Some(&one)]
        );
        assert_eq!(
            (0..=5).map(|q| heuristic.backward.get(&(q, 0))).collect::<Vec<_>>(),
            vec![Some(&w2), None, Some(&one), None, Some(&w1), None]
        );
        assert_eq!(
            (0..=5).map(|q| heuristic.backward.get(&(q, 1))).collect::<Vec<_>>(),
            vec![None, Some(&w2), None, Some(&w2), None, Some(&w2)]
        );
    }

    #[test]
    fn wrap () {
        let automaton = example_automaton();
        let heuristic = NaiveHeuristic::new(&automaton);
        
        let zero: Reverse<LogDomain<f64>> = LogDomain::zero().into();
        let one: Reverse<LogDomain<f64>> = LogDomain::one().into();
        let w1: Reverse<LogDomain<f64>> = LogDomain::new(0.25).unwrap().into();
        let w2: Reverse<LogDomain<f64>> = LogDomain::new(0.75).unwrap().into();

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
}