extern crate num_traits;

use std::cmp::{Ord, Ordering};
use std::collections::{BinaryHeap, HashMap};
use std::hash::{Hash, Hasher};
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::Mul;
use std::vec::Vec;
use self::num_traits::One;

mod from_str;

/// Configuration of an automaton containing sequence of symbols `word` to be read, a storage value `storage`, and a `weight`.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Configuration<S, T, W> {
    pub word: Vec<T>,
    pub storage: S,
    pub weight: W,
}


/// Transition of an automaton with `weight`, reading the sequence `word`, and applying the `instruction`.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Transition<A, I: Instruction<A>, T, W> {
    pub _dummy: PhantomData<A>,
    pub word: Vec<T>,
    pub weight: W,
    pub instruction: I,
}


/// Something we can `apply` to a configuration.
pub trait Instruction<A> {
    fn apply(&self, A) -> Option<A>;
}

impl<S: Hash, T: Hash, W> Hash for Configuration<S, T, W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.word.hash(state);
        self.storage.hash(state);
    }
}

impl<A: Clone, I: Instruction<A>, T: PartialEq + Clone, W: Mul<Output = W> + Copy> Transition<A, I, T, W> {
    pub fn apply(&self, c: &Configuration<A, T, W>) -> Option<Configuration<A, T, W>> {
        if !c.word.starts_with(&self.word[..]) {
            return None;
        }

        match self.instruction.apply(c.storage.clone()) {
            Some(t1) => {
                Some(Configuration {
                    word: c.word.clone().split_off(self.word.len()),
                    storage: t1,
                    weight: c.weight * self.weight,
                })
            }
            _ => None,
        }
    }
}


/// Something that has `transitions`, an `initial` configuration, and a predicate characterising terminal configurations `is_terminal`.
pub trait Automaton<S: Clone + Debug + Eq,
                    I: Clone + Debug + Eq + Instruction<S>,
                    T: Clone + Debug + Eq,
                    W: One + Mul<Output = W> + Clone + Copy + Debug + Eq + Ord>
    where Self::Key: Hash + Eq {
    type Key;

    fn extract_key(&Configuration<S, T, W>) -> &Self::Key;

    fn transitions(&self) -> &HashMap<Self::Key, BinaryHeap<Transition<S, I, T, W>>>;
    fn initial(&self) -> S;
    fn is_terminal(&self, &Configuration<S, T, W>) -> bool;
    fn recognise(&self, word: Vec<T>) -> Option<Configuration<S, T, W>> {
        let i = Configuration {
            word: word,
            storage: self.initial().clone(),
            weight: W::one(),
        };
        let mut init_heap = BinaryHeap::new();
        init_heap.push(i);

        explore(&mut init_heap,
                &(|c| Self::extract_key(&c)),
                self.transitions(),
                &(|c, r| r.apply(c)),
                &(|c| self.is_terminal(c)))
    }
}




impl<A: Eq, I: Instruction<A> + Eq, T: Eq, W: PartialOrd + Eq> PartialOrd
    for Transition<A, I, T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.weight.partial_cmp(&other.weight)
    }
}

impl<A: Eq, I: Instruction<A> + Eq, T: Eq, W: Ord + Eq> Ord for Transition<A, I, T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.weight.cmp(&other.weight)
    }
}


impl<S: Eq, T: Eq, W: PartialOrd + Eq> PartialOrd for Configuration<S, T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.weight.partial_cmp(&other.weight)
    }
}

impl<S: Eq, T: Eq, W: Ord + Eq> Ord for Configuration<S, T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.weight.cmp(&other.weight)
    }
}


/// Explores the space of all configurations in decreasing order of their weights.
/// Returns the first `accepting` configuration it encounters.
pub fn explore<C: Ord + Clone + Debug, R: Ord + Clone + Debug, K: Hash + Eq>(
    active: &mut BinaryHeap<C>,
    configuration_characteristic: &Fn(&C) -> &K,
    filtered_rules: &HashMap<K, BinaryHeap<R>>,
    apply: &Fn(&C, &R) -> Option<C>,
    accepting: &Fn(&C) -> bool)
    -> Option<C> {
    let mut i;
    let mut count = 0;

    loop {
        match active.pop() {
            Some(c) => i = c,
            _ => break,
        }

        count += 1;

        if accepting(&i) {
            println!("Considered {} configurations.", count);
            return Some(i);
        }

        for rs in filtered_rules.get(&configuration_characteristic(&i)) {
            for r in rs {
                match apply(&i, &r) {
                    Some(c1) => active.push(c1),
                    _ => (),
                }
            }
        }
    }

    None
}
