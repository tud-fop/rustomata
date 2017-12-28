use std::collections::{BTreeSet, BinaryHeap, HashMap};
use std::hash::Hash;
use std::rc::Rc;
use util::agenda::{Agenda, Capacity, PriorityQueue};
use util::push_down::Pushdown;

/// Iterator for `recognise` that creates new solutions with every step
pub struct Recogniser<'a, A, C, R: Ord, K: Hash, O> {
    // TODO rename to ParseForest
    pub agenda: A,
    pub configuration_characteristic: Box<Fn(&C) -> &K>,
    pub filtered_rules: Rc<HashMap<K, BinaryHeap<R>>>,
    pub apply: Box<Fn(&C, &R) -> Vec<C>>,
    pub accepting: Box<Fn(&C) -> bool>,
    pub item_map: Box<Fn(&(C, Pushdown<R>)) -> O + 'a>,
    pub already_found: Option<BTreeSet<C>>,
}

impl<'a, A, C, R, K, O> Iterator for Recogniser<'a, A, C, R, K, O>
where
    A: Agenda<Item = (C, Pushdown<R>)>,
    C: Clone + Ord,
    R: Clone + Ord,
    K: Eq + Hash,
{
    type Item = O;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((c, run)) = self.agenda.dequeue() {
            let founds = &mut self.already_found;
            if let Some(rs) = self.filtered_rules
                .get((self.configuration_characteristic)(&c))
            {
                for r in rs {
                    for c1 in (self.apply)(&c, r).into_iter().filter(|c1| match *founds {
                        None => true,
                        Some(ref mut set) => set.insert(c1.clone()),
                    }) {
                        let run1 = run.clone().push(r.clone());
                        self.agenda.enqueue((c1, run1));
                    }
                }
            }
            if (self.accepting)(&c) {
                return Some((self.item_map)(&(c, run)));
            }
        }

        None
    }
}

pub enum Search<'a, A, I>
where
    A: Agenda<Item = I> + 'a,
{
    All(A, Box<Fn(&I) -> Vec<I> + 'a>),
    Uniques(A, Box<Fn(&I) -> Vec<I> + 'a>, BTreeSet<I>),
}

impl<'a, A, I> Search<'a, A, I>
where
    A: Agenda<Item = I> + 'a,
    I: Ord
{
    pub fn uniques(self) -> Self {
        match self {
            Search::All(agenda, succ) | Search::Uniques(agenda, succ, _) => {
                Search::Uniques(agenda, succ, BTreeSet::new())
            }
        }
    }

    pub fn all(self) -> Self {
        match self {
            Search::All(agenda, succ) | Search::Uniques(agenda, succ, _) => {
                Search::All(agenda, succ)
            }
        }
    }
}

impl<'a, I, W> Search<'a, PriorityQueue<'a, W, I>, I>
where
    I: Clone + Ord,
    W: Ord + Clone,
{
    pub fn weighted<C>(
        init: C,
        successors: Box<Fn(&I) -> Vec<I> + 'a>,
        weight: Box<Fn(&I) -> W + 'a>,
    ) -> Self
    where
        C: IntoIterator<Item = I>,
    {
        let mut agenda = PriorityQueue::new(Capacity::Infinite, weight);

        for item in init {
            agenda.enqueue(item);
        }

        Search::All(agenda, successors)
    }

    pub fn beam(mut self, b: usize) -> Self {
        match *(&mut self) {
            Search::All(ref mut a, _) | Search::Uniques(ref mut a, _, _) => {
                a.set_capacity(b);
            }
        }
        self
    }
}

impl<'a, I> Search<'a, PriorityQueue<'a, usize, I>, I>
where
    I: Clone + Ord,
{
    pub fn unweighted<C>(init: C, successors: Box<Fn(&I) -> Vec<I> + 'a>) -> Self
    where
        C: IntoIterator<Item = I>,
    {
        Search::weighted(init, successors, Box::new(|_| 1))
    }
}

impl<'a, A, I> Iterator for Search<'a, A, I>
where
    I: Clone + Ord + ::std::fmt::Debug,
    A: Agenda<Item = I> + 'a
{
    type Item = I;
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Search::All(ref mut agenda, ref succ) => {
                while let Some(item) = Agenda::dequeue(agenda) {
                    for succ_item in (succ)(&item) {
                        agenda.enqueue(succ_item);
                    }
                    return Some(item)
                }
                return None
            }
            
            Search::Uniques(ref mut agenda, ref succ, ref mut found) => {
                while let Some(item) = Agenda::dequeue(agenda) {
                    if found.insert(item.clone()) {
                        for succ_item in (succ)(&item) {
                            agenda.enqueue(succ_item);
                        }
                        return Some(item)
                    }
                }
                return None
            }
        }
    }
}
