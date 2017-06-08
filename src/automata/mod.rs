extern crate num_traits;

use std::cmp::Ord;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::fmt::Debug;
use std::ops::Mul;
use std::vec::Vec;
use self::num_traits::One;

mod from_str;
mod configuration;
mod transition;
pub mod red;

pub use self::configuration::Configuration;
pub use self::transition::Transition;


/// Something we can `apply` to a configuration.
pub trait Instruction<A> {
    fn apply(&self, A) -> Vec<A>;
}

/// Something that has `transitions`, an `initial` configuration, and a predicate characterising terminal configurations `is_terminal`.
pub trait Automaton<S: Clone + Debug + Eq,
                    I: Clone + Debug + Eq + Instruction<S>,
                    T: Clone + Debug + Eq,
                    W: One + Mul<Output = W> + Clone + Copy + Debug + Eq + Ord>
    where Self::Key: Hash + Eq +Clone{
    type Key;

    fn extract_key(&Configuration<S, T, W>) -> &Self::Key;

    fn transitions(&self) -> &HashMap<Self::Key, BinaryHeap<Transition<S, I, T, W>>>;
    fn states(&self) -> Vec<Self::Key>{
        let mut st=Vec::new();
        for (k,_) in self.transitions(){
            st.push(k.clone())
        }
        st
    }
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

/// Explores the space of all configurations in decreasing order of their weights.
/// Returns the first `accepting` configuration it encounters.
pub fn explore<C: Ord + Clone + Debug, R: Ord + Clone + Debug, K: Hash + Eq>(
    active: &mut BinaryHeap<C>,
    configuration_characteristic: &Fn(&C) -> &K,
    filtered_rules: &HashMap<K, BinaryHeap<R>>,
    apply: &Fn(&C, &R) -> Vec<C>,
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
        println!("{:?}", i);
        if accepting(&i) {
            println!("Considered {} configurations.", count);
            return Some(i);
        }

        for rs in filtered_rules.get(&configuration_characteristic(&i)) {
            for r in rs {
                println!("{:?}", r);
                active.extend(apply(&i, &r))
            }
        }
    }

    None
}
