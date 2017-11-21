extern crate num_traits;

use std::cmp::Ord;
use std::collections::{BinaryHeap, HashMap};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Mul;
use std::vec::Vec;

use self::num_traits::One;

use util::push_down::Pushdown;
use util::agenda::{Agenda, BoundedPriorityQueue, Weighted};

pub mod from_str;
pub mod configuration;
pub mod transition;
pub mod red;

pub use self::configuration::Configuration;
pub use self::transition::Transition;
pub use self::red::TransitionKey;
use coarse_to_fine::*;

/// Something we can `apply` to a configuration.
pub trait Instruction<A> {
    fn apply(&self, A) -> Vec<A>;
}

// items of the transition system
type Item<S, I, T, W> = (Configuration<S, T, W>, Pushdown<Transition<S, I, T, W>>);
pub type VecItem<S, I, T, W> = (Configuration<S, T, W>, Vec<Transition<S, I, T, W>>);

// map from key to transition
type TransitionMap<K, S, I, T, W> = HashMap<K, BinaryHeap<Transition<S, I, T, W>>>;

// kinds of recognisers
type ExactRecogniser<'a, S, I, T, W, K> = Recogniser<'a, BinaryHeap<Item<S, I, T, W>>, Configuration<S, T, W>, Transition<S, I, T, W>, K>;
type BeamRecogniser<'a, S, I, T, W, K> = Recogniser<'a, BoundedPriorityQueue<W, Item<S, I, T, W>>, Configuration<S, T, W>, Transition<S, I, T, W>, K>;

impl<S, I: Instruction<S>, T, W: Clone> Weighted for Item<S, I, T, W> {
    type Weight = W;

    fn get_weight(&self) -> W {
        self.0.weight.clone()
    }
}

/// Something that has `transitions`, an `initial` configuration, and a predicate characterising terminal configurations `is_terminal`.
pub trait Automaton<S: Clone + Debug + Eq,
                    I: Clone + Debug + Eq + Instruction<S>,
                    T: Clone + Debug + Eq,
                    W: One + Mul<Output = W> + Clone + Copy + Debug + Eq + Ord>
    where Self::Key: Hash + Eq + Clone {
    type Key;

    fn extract_key(&Configuration<S, T, W>) -> &Self::Key;

    fn transitions(&self) -> &TransitionMap<Self::Key, S, I, T, W>;

    fn keys(&self) -> Vec<Self::Key> {
        self.transitions().keys().cloned().collect()
    }

    fn initial(&self) -> S;

    fn is_terminal(&self, &Configuration<S, T, W>) -> bool;

    fn recognise(&self, word: Vec<T>) -> ExactRecogniser<S, I, T, W, Self::Key> {
        let i = Configuration {
            word: word,
            storage: self.initial().clone(),
            weight: W::one(),
        };
        let mut init_heap = BinaryHeap::new();
        init_heap.push((i, Pushdown::new()));

        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| Self::extract_key(c)),
            filtered_rules: self.transitions().clone(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(move |c| self.is_terminal(c))
        }
    }

    fn recognise_beam_search(&self, beam_width: usize, word: Vec<T>) -> BeamRecogniser<S, I, T, W, Self::Key> {
        let i = Configuration {
            word: word,
            storage: self.initial().clone(),
            weight: W::one(),
        };
        let mut init_heap = BoundedPriorityQueue::new(beam_width);
        init_heap.enqueue((i, Pushdown::new()));

        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| Self::extract_key(c)),
            filtered_rules: self.transitions().clone(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(move |c| self.is_terminal(c))
        }
    }

    fn check_run(&self, run: &[Transition<S, I, T, W>]) -> Option<VecItem<S, I, T, W>> {
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
    fn check<'a>(&'a self, storage: S, run: &[Transition<S, I, T, W>]) -> Vec<S> {
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

/// Iterator for `recognise` that creates new solutions with every step
pub struct Recogniser<'a, A: Agenda<Item=(C, Pushdown<R>)>, C: Ord, R: Ord, K: Hash> {
    agenda: A,
    configuration_characteristic: Box<FnMut(&C) -> &K>,
    filtered_rules: HashMap<K, BinaryHeap<R>>,
    apply: Box<FnMut(&C, &R) -> Vec<C>>,
    accepting: Box<FnMut(&C) -> bool + 'a>
}

impl<'a, A: Agenda<Item=(C, Pushdown<R>)>, C: Ord + Clone + Debug, R: Ord + Clone + Debug, K: Hash + Eq> Iterator for Recogniser<'a, A, C, R, K> {
    type Item = (C, Pushdown<R>);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((c, run)) = self.agenda.dequeue() {
            if let Some(rs) = self.filtered_rules.get((self.configuration_characteristic)(&c)) {
                for r in rs {
                    for c1 in (self.apply)(&c, r) {
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
