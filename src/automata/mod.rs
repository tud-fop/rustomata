extern crate num_traits;

use std::cmp::Ord;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::io::{self,Write};
use std::fmt::Debug;
use std::ops::Mul;
use std::time::SystemTime;
use std::vec::Vec;
use self::num_traits::One;

use push_down::Pushdown;
use util::agenda::{Agenda, BoundedPriorityQueue, Weighted};

pub mod from_str;
pub mod configuration;
pub mod transition;
pub mod red;

pub use self::configuration::Configuration;
pub use self::transition::Transition;
pub use self::red::TransitionKey;
use util::ctf::*;

/// Something we can `apply` to a configuration.
pub trait Instruction<A> {
    fn apply(&self, A) -> Vec<A>;
}

type Item<S, I, T, W> = (Configuration<S, T, W>, Pushdown<Transition<S, I, T, W>>);

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

    fn transitions(&self) -> &HashMap<Self::Key, BinaryHeap<Transition<S, I, T, W>>>;

    fn keys(&self) -> Vec<Self::Key> {
        self.transitions().keys().map(|k| k.clone()).collect()
    }

    fn initial(&self) -> S;

    fn is_terminal(&self, &Configuration<S, T, W>) -> bool;

    fn recognise<'a>(&'a self, word: Vec<T>) -> Recogniser<'a, BinaryHeap<Item<S, I, T, W>>, Configuration<S, T, W>, Transition<S, I, T, W>, Self::Key> {
        let i = Configuration {
            word: word,
            storage: self.initial().clone(),
            weight: W::one(),
        };
        let mut init_heap = BinaryHeap::new();
        init_heap.push((i, Pushdown::new()));

        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| Self::extract_key(&c)),
            filtered_rules: self.transitions().clone(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(move |c| self.is_terminal(c))
        }
    }

    fn recognise_beam_search<'a>(&'a self, beam_width: usize, word: Vec<T>) -> Recogniser<'a, BoundedPriorityQueue<W, Item<S, I, T, W>>, Configuration<S, T, W>, Transition<S, I, T, W>, Self::Key> {
        let i = Configuration {
            word: word,
            storage: self.initial().clone(),
            weight: W::one(),
        };
        let mut init_heap = BoundedPriorityQueue::new(beam_width);
        init_heap.enqueue((i, Pushdown::new()));

        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| Self::extract_key(&c)),
            filtered_rules: self.transitions().clone(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(move |c| self.is_terminal(c))
        }
    }

    fn check_run(&self, run: &Vec<Transition<S, I, T, W>>) -> Option<(Configuration<S, T, W>, Vec<Transition<S, I, T, W>>)> {
        let heap = self.check(self.initial().clone(), run);
        if heap.is_empty(){
            return None;
        }
        let c = Configuration {
            word: run_word(run),
            storage: heap[0].clone(),
            weight: run_weight(run),
        };
        Some((c, run.clone()))
    }

    //note: gives back the first configuration it finds
    fn check<'a>(&'a self, storage: S, run: &Vec<Transition<S, I, T, W>>) -> Vec<S> {
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
        let mut i = 0;
        let now = SystemTime::now();
        while let Some((c, run)) = self.agenda.dequeue() {
            i = i + 1;
            for rs in self.filtered_rules.get(&(self.configuration_characteristic)(&c)) {
                for r in rs {
                    for c1 in (self.apply)(&c, &r) {
                        let run1 = run.clone().push(r.clone());
                        self.agenda.enqueue((c1, run1));
                    }
                }
            }
            if (self.accepting)(&c) {
                match now.elapsed() {
                    Ok(elapsed) => {
                        writeln!(io::stderr(), "New successful configuration found after inspecting {} configurations in {:.3}s.", i, elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9).unwrap();
                    },
                    Err(_) => {
                        writeln!(io::stderr(), "New successful configuration found after inspecting {} configurations.", i).unwrap();
                    },
                };
                return Some((c, run));
            }
        }

        match now.elapsed() {
            Ok(elapsed) => {
                writeln!(io::stderr(), "No new successful configuration found after inspecting {} configurations in {:.3}s.", i, elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9).unwrap();
            },
            Err(_) => {
                writeln!(io::stderr(), "No new successful configuration found after inspecting {} configurations.", i).unwrap();
            },
        }
        None
    }
}
