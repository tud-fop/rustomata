use num_traits::One;
use std::collections::{BinaryHeap, BTreeMap};
use std::ops::MulAssign;

use recognisable::{Instruction, Transition};
use recognisable::automaton::Automaton;
use util::push_down::Pushdown;

pub mod equivalence_classes;
pub mod relabel;
pub mod tts;

/// Object defining the strategies used for `approximation`
pub trait ApproximationStrategy<T, W>: Sized
    where Self::I1: Clone + Eq + Instruction + Ord,
          Self::I2: Clone + Eq + Instruction + Ord,
          Self::A1: Automaton<T, W, I=Self::I1>,
          Self::A2: Automaton<T, W, I=Self::I2> + Sized,
          T: Clone + Eq + Ord,
          W: Clone + MulAssign + One + Ord,
{
    type I1;
    type I2;
    type A1;
    type A2;

    fn approximate_storage(&self, <Self::I1 as Instruction>::Storage) -> <Self::I2 as Instruction>::Storage;

    fn approximate_instruction(&self, &Self::I1) -> Self::I2;

    fn approximate_automaton(self, automaton1: &Self::A1)
                             -> (Self::A2, ApproximationInstance<Self, T, W>)
    {
        let mut instance = ApproximationInstance::new(self);
        let transitions2: Vec<_> = automaton1.transitions().map(|t| instance.approximate_transition(t.clone())).collect();
        let initial2 = instance.approximate_storage(automaton1.initial());

        (Self::A2::from_transitions(transitions2, initial2), instance)
    }
}

pub struct ApproximationInstance<Strategy, T, W>
    where Strategy: ApproximationStrategy<T, W>,
          T: Clone + Eq + Ord,
          W: Clone + MulAssign + One + Ord,
{
    reverse_transition_map: BTreeMap<Transition<Strategy::I2, T, W>, Vec<Transition<Strategy::I1, T, W>>>,
    strategy: Strategy,
}

/// An instance of an ApproximationStrategy that remembers the approximated transitions.
impl<Strategy, T, W> ApproximationInstance<Strategy, T, W>
    where Strategy: ApproximationStrategy<T, W>,
          Strategy::I2: Clone + Eq + Ord,
          Strategy::I1: Clone + Eq + Ord,
          T: Clone + Eq + Ord,
          W: Clone + MulAssign + One + Ord,
{
    pub fn new(strategy: Strategy) -> Self {
        ApproximationInstance {
            reverse_transition_map: BTreeMap::new(),
            strategy,
        }
    }

    pub fn approximate_storage(&self, s1: <Strategy::I1 as Instruction>::Storage)
                               -> <Strategy::I2 as Instruction>::Storage
    {
        self.strategy.approximate_storage(s1)
    }

    pub fn approximate_instruction(&self, i1: &Strategy::I1) -> Strategy::I2 {
        self.strategy.approximate_instruction(i1)
    }

    pub fn approximate_transition(&mut self, t1: Transition<Strategy::I1, T, W>)
                                  -> Transition<Strategy::I2, T, W>
    {
        let t2 = Transition { word: t1.word.clone(),
                              instruction: self.approximate_instruction(&t1.instruction),
                              weight: t1.weight.clone(), };
        self.reverse_transition_map.entry(t2.clone()).or_insert(Vec::new()).push(t1);
        t2
    }

    pub fn unapproximate_transition(&self, t2: &Transition<Strategy::I2, T, W>)
                                    -> Vec<Transition<Strategy::I1, T, W>>
    {
        match self.reverse_transition_map.get(t2) {
            None => Vec::new(),
            Some(v) => v.clone(),
        }
    }

    pub fn unapproximate_run(&self, run2: Pushdown<Transition<Strategy::I2, T, W>>)
                             -> BinaryHeap<Pushdown<Transition<Strategy::I1, T, W>>>
    {
        let f = |h: BinaryHeap<Pushdown<_>>, ts1: Vec<_>| {
            let new_runs: Vec<Pushdown<_>> =
                h.into_iter()
                .flat_map( |run: Pushdown<_>| -> Vec<Pushdown<_>>{
                    let push_transition = |t1: &Transition<_, _, _>| {
                        run.clone().push(t1.clone())
                    };
                    ts1.iter().map(push_transition).collect()
                } ).collect();
            BinaryHeap::from(new_runs)
        };

        let initial_heap = BinaryHeap::from(vec![Pushdown::Empty]);
        run2.iter().map(|t2| self.unapproximate_transition(&t2)).fold(initial_heap, f)
    }

}
