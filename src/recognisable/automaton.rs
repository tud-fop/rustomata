extern crate num_traits;

use std::cmp::Ord;
use std::collections::{BinaryHeap, HashMap};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Mul;
use std::vec::Vec;

use num_traits::One;

use coarse_to_fine::{run_weight, run_word};
use recognisable::{Configuration, Instruction, Recogniser, Transition};
use util::push_down::Pushdown;
use util::agenda::{Agenda, BoundedPriorityQueue};


// items of the transition system
pub type Item<S, I, T, W> = (Configuration<S, T, W>, Pushdown<Transition<I, T, W>>);
pub type VecItem<S, I, T, W> = (Configuration<S, T, W>, Vec<Transition<I, T, W>>);

// map from key to transition
pub type TransitionMap<K, I, T, W> = HashMap<K, BinaryHeap<Transition<I, T, W>>>;


/// Something that has `transitions`, an `initial` configuration, and a predicate characterising terminal configurations `is_terminal`.
pub trait Automaton<T, W>
    where Self::I: Clone + Debug + Eq + Instruction,
          T: Clone + Debug + Eq,
          W: One + Mul<Output = W> + Clone + Copy + Debug + Eq + Ord,
          <Self::I as Instruction>::Storage: Clone + Debug + Eq,
          Self::Key: Hash + Eq + Clone,
{
    type I;   /// instructions
    type Key;

    fn extract_key(&Configuration<<Self::I as Instruction>::Storage, T, W>) -> &Self::Key;

    fn transitions(&self) -> TransitionMap<Self::Key, Self::I, T, W>;

    fn keys(&self) -> Vec<Self::Key> {
        self.transitions().keys().cloned().collect()
    }

    fn initial(&self) -> <Self::I as Instruction>::Storage;

    fn is_terminal(&Configuration<<Self::I as Instruction>::Storage, T, W>) -> bool;

    fn check_run(&self, run: &[Transition<Self::I, T, W>]) -> Option<VecItem<<Self::I as Instruction>::Storage, Self::I, T, W>> {
        let heap = self.check(self.initial().clone(), run);
        if heap.is_empty(){
            return None;
        }
        let c = Configuration {
            word: run_word(run),
            storage: heap[0].clone(),
            weight: run_weight(run),
        };
        Some((c, run.to_owned()))
    }

    //note: gives back the first configuration it finds
    fn check<'b>(&'b self, storage: <Self::I as Instruction>::Storage, run: &[Transition<Self::I, T, W>]) -> Vec<<Self::I as Instruction>::Storage> {
        let mut storage_heap = Vec::new();
        storage_heap.push(storage);
        for t in run {
            let mut new_storage_heap = Vec::new();
            for s in storage_heap{
                for s1 in t.instruction.apply(s) {
                    new_storage_heap.push(s1);
                }
            }
            storage_heap = new_storage_heap;
        }
        storage_heap
    }
}

// kinds of recognisers
type ExactRecogniser<'a, S, I, T, W, K> = Recogniser<'a, BinaryHeap<Item<S, I, T, W>>, Configuration<S, T, W>, Transition<I, T, W>, K, Item<S, I, T, W>>;

pub fn recognise<'a, A, T, W>(a: &'a A, word: Vec<T>) -> Box<ExactRecogniser<'a, <A::I as Instruction>::Storage, A::I, T, W, A::Key>>
    where A: Automaton<T, W>,
          A::Key: Hash,
          A::I: Clone + Debug + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Debug + Eq + Ord,
          T: Clone + Debug + Eq + Ord,
          W: Copy + Debug + One + Ord,
{
    let i = Configuration {
        word: word,
        storage: a.initial(),
        weight: W::one(),
    };
    let mut init_heap = BinaryHeap::new();
    init_heap.enqueue((i, Pushdown::new()));

    Box::new(
        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| A::extract_key(c)),
            filtered_rules: a.transitions(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(|c| A::is_terminal(c)),
            item_map: Box::new(|i| i.clone()),
        }
    )
}

type BeamRecogniser<'a, S, I, T, W, K> = Recogniser<'a, BoundedPriorityQueue<W, Item<S, I, T, W>>, Configuration<S, T, W>, Transition<I, T, W>, K, Item<S, I, T, W>>;

pub fn recognise_beam<'a, A, T, W>(a: &'a A, beam: usize, word: Vec<T>) -> Box<BeamRecogniser<'a, <A::I as Instruction>::Storage, A::I, T, W, A::Key>>
    where A: Automaton<T, W>,
          A::Key: Hash,
          A::I: Clone + Debug + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Debug + Eq + Ord,
          T: Clone + Debug + Eq + Ord,
          W: Copy + Debug + One + Ord,
{
    let i = Configuration {
        word: word,
        storage: a.initial(),
        weight: W::one(),
    };
    let mut init_heap = BoundedPriorityQueue::new(beam);
    init_heap.enqueue((i, Pushdown::new()));

    Box::new(
        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| A::extract_key(c)),
            filtered_rules: a.transitions(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(|c| A::is_terminal(c)),
            item_map: Box::new(|i| i.clone()),
        }
    )
}

