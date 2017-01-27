use std::cmp::Ord;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
// use std::collections::BTreeSet;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::Add;
use std::vec::Vec;

mod from_str;

/// Configuration of an automaton containing sequence of symbols `word` to be read, a storage value `storage`, and a `weight`.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Configuration<S, T, W> {
    pub word: Vec<T>,
    pub storage: S,
    pub weight: W
}


/// Transition of an automaton with `weight`, reading the sequence `word`, and applying the `instruction`.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Transition<A, I: Instruction<A>, T, W> {
    pub _dummy: PhantomData<A>,
    pub word: Vec<T>,
    pub weight: W,
    pub instruction: I
}


/// Something we can `apply` to a configuration.
pub trait Instruction<A> {
    fn apply(&self, A) -> Option<A>;
}


impl<A: Clone, I: Instruction<A>, T: PartialEq + Clone, W: Add<Output=W> + Copy> Transition<A, I, T, W> {
    pub fn apply(&self, c: &Configuration<A, T, W>) -> Option<Configuration<A, T, W>> {
        if !c.word.starts_with(&self.word[..]) {
            return None;
        }

        match self.instruction.apply(c.storage.clone()) {
            Some(t1) =>
                Some( Configuration { word: c.word.clone().split_off(self.word.len()),
                                      storage: t1,
                                      weight: c.weight + self.weight } ),
            _        => None
        }
    }
}


/// Something that has `transitions`, an `initial` configuration, and a predicate characterising terminal configurations `is_terminal`.
pub trait Automaton<S: Clone + Debug + Eq,
                    I: Clone + Debug + Eq + Instruction<S>,
                    T: Clone + Debug + Eq,
                    W: Add<Output=W> + Clone + Copy + Debug + Eq + Ord> {
    fn transitions(&self) -> Vec<Transition<S, I, T, W>>;
    fn initial(&self) -> S;
    fn is_terminal(&self, &Configuration<S, T, W>) -> bool;
    fn recognise(&self, word: Vec<T>, zero: W) -> Option<Configuration<S, T, W>> {
        let i = Configuration { word: word, storage: self.initial().clone(), weight: zero };
        let mut init_heap = BinaryHeap::new();
        let mut transition_heap = BinaryHeap::new();

        init_heap.push(i);
        for t in self.transitions() {
            transition_heap.push(t)
        }

        explore(
            &mut init_heap,
            &transition_heap,
            &(|c, r| r.apply(c)),
            &(|c| self.is_terminal(c))
        )
    }
}




impl<A: Eq, I: Instruction<A> + Eq, T: Eq, W: PartialOrd + Eq> PartialOrd for Transition<A, I, T, W> {
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
pub fn explore<C: Ord + Clone + Debug, R: Ord + Clone + Debug>
    ( active:      &mut BinaryHeap<C>,
      rules:       &BinaryHeap<R>,
      apply:       &Fn(&C, &R) -> Option<C>,
      accepting:   &Fn(&C) -> bool
    ) -> Option<C> {
//        let mut passive = BTreeSet::new();
        let mut i;
        let mut count = 0;

        loop {
            match active.pop() {
                Some(c) => i = c,
                _       => break
            }

            count += 1;

            if accepting(&i) {
                println!("Considered {} configurations.", count);
                return Some(i);
            }

//            passive.insert(i.clone());

            let mut new_configurations = BinaryHeap::new();
            for r in rules {
                match apply(&i, r) {
                    Some(c1) => new_configurations.push(c1),
                    _ => ()
                }
            }

            active.append(&mut new_configurations);
        }

        None
}
