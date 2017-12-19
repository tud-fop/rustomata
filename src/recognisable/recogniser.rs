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

pub struct Search<'a, I, W> {
    agenda: PriorityQueue<'a, W, I>,
    successors: Box<Fn(&I) -> Vec<I> + 'a>,
    found: BTreeSet<I>,
}

pub struct UniqueSearch<'a, I, W> {
    agenda: PriorityQueue<'a, W, I>,
    successors: Box<Fn(&I) -> Vec<I> + 'a>,
}

impl<'a, I, W> Search<'a, I, W>
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
        let mut found = BTreeSet::new();
        for item in init {
            found.insert(item.clone());
            agenda.enqueue(item);
        }
        Search {
            agenda,
            found,
            successors,
        }
    }
}

impl<'a, I, W> UniqueSearch<'a, I, W>
where
    W: Clone + Ord
{
    pub fn weighted<C>(
        init: C,
        successors: Box<Fn(&I) -> Vec<I> + 'a>,
        weight: Box<Fn(&I) -> W + 'a>,
        cap: Capacity
    ) -> Self
    where C: IntoIterator<Item=I> {
        let mut agenda = PriorityQueue::new(cap, weight);
        for item in init {
            agenda.enqueue(item);
        }
        UniqueSearch {
            agenda,
            successors,
        }
    }
}

impl<'a, I> Search<'a, I, usize>
where
    I: Clone + Ord,
{
    pub fn unweighted<C>(init: C, successors: Box<Fn(&I) -> Vec<I> + 'a>) -> Self
    where
        C: IntoIterator<Item = I>,
    {
        Search::weighted(init, successors, Box::new(|_| 0))
    }
}

impl<'a, I, W> Iterator for Search<'a, I, W>
where
    W: Ord + Clone,
    I: Clone + Ord,
{
    type Item = I;
    fn next(&mut self) -> Option<Self::Item> {
        let &mut Search {
            ref mut agenda,
            ref successors,
            ref mut found,
        } = self;

        while let Some(item) = Agenda::dequeue(agenda) {
            for succ in (successors)(&item)
                .into_iter()
                .filter(| i | found.insert(i.clone()))
            {
                agenda.enqueue(succ);
            }
            return Some(item);
        }

        None
    }
}

impl<'a, I, W> Iterator for UniqueSearch<'a, I, W>
where
    W: Ord + Clone,
{
    type Item = I;
    fn next(&mut self) -> Option<Self::Item> {
        let &mut UniqueSearch {
            ref mut agenda,
            ref successors,
        } = self;

        while let Some(item) = Agenda::dequeue(agenda) {
            for succ in (successors)(&item) {
                agenda.enqueue(succ);
            }
            return Some(item);
        }

        None
    }
}