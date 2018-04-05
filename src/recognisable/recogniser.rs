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
    pub accepting: Box<Fn(&C) -> bool + 'a>,
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

/// Implements a `Search` in a graph that is defined by a set of elements and a successor function.
pub enum Search<A, I, F>
where
    A: Agenda<Item = I>,
    F: FnMut(&I) -> Vec<I>,
{
    /// Yield elements twice if there are multiple paths to them.
    All(A, F),
    /// Stores the set of all expanded elements to not yield them multiple times.
    Uniques(A, F, BTreeSet<I>),
}

impl<A, I, F> Search<A, I, F>
where
    A: Agenda<Item = I>,
    I: Ord,
    F: FnMut(&I) -> Vec<I>,
{
    /// Stores the set of all expanded elements to not yield them multiple times.
    pub fn uniques(self) -> Self {
        match self {
            Search::All(agenda, succ) | Search::Uniques(agenda, succ, _) => {
                Search::Uniques(agenda, succ, BTreeSet::new())
            }
        }
    }

    /// Yield elements twice if there are multiple paths to them.
    pub fn all(self) -> Self {
        match self {
            Search::All(agenda, succ) | Search::Uniques(agenda, succ, _) => {
                Search::All(agenda, succ)
            }
        }
    }
}

impl<I, W, F> Search<PriorityQueue<W, I>, I, F>
where
    I: Clone + Ord + Weighted<Weight=W>,
    W: Ord + Clone,
    F: FnMut(&I) -> Vec<I>,
{
    /// Initializes a weighted `Search` using a `PriorityQueue`.
    pub fn weighted<C>(init: C, successors: F) -> Self
    where
        C: IntoIterator<Item = I>,
    {
        let mut agenda = PriorityQueue::new(Capacity::Infinite);

        for item in init {
            agenda.enqueue(item);
        }

        Search::All(agenda, successors)
    }

    /// Switch to beam search with a beam `width`.
    pub fn beam(mut self, width: Capacity) -> Self {
        if let Capacity::Limit(b) = width {
            match &mut self {
                &mut Search::All(ref mut a, _) | &mut Search::Uniques(ref mut a, _, _) => {
                    a.set_capacity(b);
                }
            }
        }

        self
    }
}

impl<I, F> Search<Vec<I>, I, F>
where
    I: Clone + Ord,
    F: FnMut(&I) -> Vec<I>,
{
    /// Initialize an unweighted `Search` using a `Vec` as `Agenda`.
    pub fn unweighted<C>(init: C, successors: F) -> Self
    where
        C: IntoIterator<Item = I>,
    {
        Search::All(init.into_iter().collect(), successors)
    }
}

impl<A, I, F> Iterator for Search<A, I, F>
where
    I: Clone + Ord,
    A: Agenda<Item = I>,
    F: FnMut(&I) -> Vec<I>,
{
    type Item = I;
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Search::All(ref mut agenda, ref mut succ) => {
                if let Some(item) = Agenda::dequeue(agenda) {
                    for succ_item in (succ)(&item) {
                        agenda.enqueue(succ_item);
                    }
                    return Some(item);
                }
                None
            }

            Search::Uniques(ref mut agenda, ref mut succ, ref mut found) => {
                while let Some(item) = Agenda::dequeue(agenda) {
                    if found.insert(item.clone()) {
                        for succ_item in (succ)(&item).into_iter().filter(|i| !found.contains(i)) {
                            agenda.enqueue(succ_item);
                        }
                        return Some(item);
                    }
                }
                None
            }
        }
    }
}

use std::cmp::Ordering;
use util::agenda::Weighted;

/// A tuple consisting of an item of type `I` and a weight of type `W`.
/// `Eq` and `Ord` impls only consider the item.
#[derive(Debug, Clone, Copy)]
pub struct WeightedSearchItem<I, W>(pub I, pub W);
impl<I, W> PartialEq for WeightedSearchItem<I, W>
where
    I: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<I, W> Eq for WeightedSearchItem<I, W>
where
    I: Ord,
{
}
impl<I, W> PartialOrd for WeightedSearchItem<I, W>
where
    I: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<I, W> Ord for WeightedSearchItem<I, W>
where
    I: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}
impl<I, W> Weighted for WeightedSearchItem<I, W> where W: Copy {
    type Weight = W;
    fn get_weight(&self) -> W {
        match *self {
            WeightedSearchItem(_, ref w) => *w
        }
    }
}