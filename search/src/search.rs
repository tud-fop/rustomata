//! This module provides the `Search` type, an `Iterator` over nodes in a graph
//! that is explored as an object of this type iterates.

use crate::agenda::{binary_heap::weighted::BinaryHeap, weighted::Weighted, Agenda};
use std::collections::VecDeque;

/// An `Iterator` exploring a graph with current frontier nodes in `agenda`
/// and a successor relation.
pub struct Search<A, S, II>
where
    A: Agenda,
    S: FnMut(&A::Item) -> II,
    II: IntoIterator<Item = A::Item>,
{
    agenda: A,
    successors: S,
}

impl<A, S, II> Search<A, S, II>
where
    A: Agenda,
    S: FnMut(&A::Item) -> II,
    II: IntoIterator<Item = A::Item>,
{
    /// Constructs a `Search` with a given `Agenda` object.
    pub fn with_agenda(agenda: A, successors: S) -> Self {
        Search { agenda, successors }
    }
}

impl<A, S, II> Search<A, S, II>
where
    A: Agenda,
    S: FnMut(&A::Item) -> II,
    II: IntoIterator<Item = A::Item>,
    A::Item: Ord,
{
    /// Transforms the `Search` into a `unique::Search`. This will enumerate
    /// only unique nodes (respecting the implementation of `Ord`) in a graph.
    pub fn uniques(self) -> unique::Search<A, S, II> {
        unique::Search::from_search(self)
    }
}

impl<I, S, II> Search<Vec<I>, S, II>
where
    S: FnMut(&I) -> II,
    II: IntoIterator<Item = I>,
{
    /// Performs a depth-first search.
    pub fn dfs<II2: IntoIterator<Item = I>>(initials: II2, successors: S) -> Self {
        Search::with_agenda(initials.into_iter().collect::<Vec<_>>(), successors)
    }
}

impl<I, S, II> Search<VecDeque<I>, S, II>
where
    S: FnMut(&I) -> II,
    II: IntoIterator<Item = I>,
{
    /// Performs a breadth-first search.
    pub fn bfs<II2: IntoIterator<Item = I>>(initials: II2, successors: S) -> Self {
        Search::with_agenda(initials.into_iter().collect::<VecDeque<_>>(), successors)
    }
}

impl<I: Weighted, S, II> Search<BinaryHeap<I>, S, II>
where
    I::Weight: Ord,
    S: FnMut(&I) -> II,
    II: IntoIterator<Item = I>,
{
    /// Performs a Dijkstra search.
    pub fn weighted<II2: IntoIterator<Item = I>>(initials: II2, successors: S) -> Self {
        Search::with_agenda(initials.into_iter().collect::<BinaryHeap<_>>(), successors)
    }
}

impl<A, S, II> Iterator for Search<A, S, II>
where
    A: Agenda,
    S: FnMut(&A::Item) -> II,
    II: IntoIterator<Item = A::Item>,
{
    type Item = A::Item;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.agenda.pop() {
            self.agenda.extend((self.successors)(&item));
            Some(item)
        } else {
            None
        }
    }
}

pub mod unique {
    use crate::agenda::Agenda;
    use std::collections::BTreeSet;

    /// This is an adapter for `super::Search` that filters nodes that already
    /// occured during the iteration.
    pub struct Search<A, S, II>
    where
        A: Agenda,
        S: FnMut(&A::Item) -> II,
        II: IntoIterator<Item = A::Item>,
    {
        search: super::Search<A, S, II>,
        elements: BTreeSet<A::Item>,
    }

    impl<A, S, II> Search<A, S, II>
    where
        A: Agenda,
        S: FnMut(&A::Item) -> II,
        II: IntoIterator<Item = A::Item>,
        A::Item: Ord,
    {
        /// Creates a unique `Search` from a `super::Search`.
        pub fn from_search(search: super::Search<A, S, II>) -> Self {
            Search {
                search,
                elements: BTreeSet::new(),
            }
        }
    }

    impl<A, S, II> Iterator for Search<A, S, II>
    where
        A: Agenda,
        S: FnMut(&A::Item) -> II,
        II: IntoIterator<Item = A::Item>,
        A::Item: Ord + Clone,
    {
        type Item = A::Item;
        fn next(&mut self) -> Option<Self::Item> {
            let &mut Search {
                ref mut search,
                ref mut elements,
            } = self;

            if let Some(item) = search.agenda.pop() {
                if elements.insert(item.clone()) {
                    search.agenda.extend(
                        (search.successors)(&item)
                            .into_iter()
                            .filter(|i| !elements.contains(i)),
                    );
                    Some(item)
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}
