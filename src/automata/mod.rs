extern crate num_traits;

use std::cmp::Ord;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::io::{self,Write};
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
    where Self::Key: Hash + Eq + Clone {
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
    fn recognise<'a>(&'a self, word: Vec<T>) -> Recogniser<'a, Configuration<S, T, W>, Transition<S, I, T, W>, Self::Key> {
        let i = Configuration {
            word: word,
            storage: self.initial().clone(),
            weight: W::one(),
        };
        let mut init_heap = BinaryHeap::new();
        init_heap.push((i, Vec::new()));

        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| Self::extract_key(&c)),
            filtered_rules: self.transitions().clone(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(move |c| self.is_terminal(c))
        }
    }

    fn check_run(&self, run: &Vec<Transition<S, I, T, W>>, word: Vec<T>) -> Option<Configuration<S, T, W>>{
        let c = Configuration {
            word: word,
            storage: self.initial().clone(),
            weight: W::one(),
        };
        self.check(c, run)
    }

    //note: gives back the first configuration it finds
    fn check<'a>(&'a self, c: Configuration<S, T, W>, run: &Vec<Transition<S, I, T, W>>) -> Option<Configuration<S, T, W>> {
        match run.first(){
            Some(t) => {
                let mut run1 = run.clone();
                run1.remove(0);
                for c1 in t.apply(&c) {
                    match self.check(c1, &run1){
                        Some(x) =>{
                            return Some(x);
                        },
                        None =>(),
                    }
                }
                None
            },
            None => Some(c.clone()),
        }
    }
}

pub struct Recogniser<'a, C: Ord, R: Ord, K: Hash> {
    agenda: BinaryHeap<(C, Vec<R>)>,
    configuration_characteristic: Box<FnMut(&C) -> &K>,
    filtered_rules: HashMap<K, BinaryHeap<R>>,
    apply: Box<FnMut(&C, &R) -> Vec<C>>,
    accepting: Box<FnMut(&C) -> bool + 'a>
}

impl<'a, C: Ord + Clone + Debug, R: Ord + Clone + Debug, K: Hash + Eq> Iterator for Recogniser<'a, C, R, K> {
    type Item = (C, Vec<R>);

    fn next(&mut self) -> Option<(C, Vec<R>)> {
        let mut i = 0;
        while let Some((c, run)) = self.agenda.pop() {
            i = i + 1;
            for rs in self.filtered_rules.get(&(self.configuration_characteristic)(&c)) {
                for r in rs {
                    for c1 in (self.apply)(&c, &r) {
                        let mut run1 = run.clone();
                        run1.push(r.clone());
                        self.agenda.push((c1, run1))
                    }
                }
            }
            if (self.accepting)(&c) {
                writeln!(io::stderr(), "New successful configuration found after inspecting {} configurations.", i).unwrap();
                return Some((c, run));
            }
        }

        writeln!(io::stderr(), "No new successful configuration found after inspecting {} configurations.", i).unwrap();
        None
    }
}
