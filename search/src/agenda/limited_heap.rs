use min_max_heap::{IntoIter, MinMaxHeap};
use std::iter::IntoIterator;

use super::weighted::WeightedItem;

/// A heap that only allows a constant amount of items. It will keep the items
/// with the highest priority.
#[derive(Clone)]
pub struct LimitedHeap<I, W>
where
    W: Ord,
{
    heap: MinMaxHeap<WeightedItem<I, W>>,
    capacity: usize,
}

impl<I, W: Ord> LimitedHeap<I, W> {
    pub fn with_capacity(capacity: usize) -> Self {
        LimitedHeap {
            heap: MinMaxHeap::with_capacity(capacity),
            capacity,
        }
    }

    pub fn push(&mut self, element: I, priority: W) -> Option<I> {
        if self.capacity > self.heap.len() {
            self.heap.push(WeightedItem(element, priority));
            None
        } else {
            Some(self.heap.push_pop_min(WeightedItem(element, priority)).0)
        }
    }

    pub fn pop(&mut self) -> Option<I> {
        self.heap.pop_max().map(|wi| wi.0)
    }

    pub fn clear(&mut self) {
        self.heap.clear();
    }

    pub fn len(&self) -> usize {
        self.heap.len()
    }

    pub fn peek(&self) -> Option<&I> {
        self.heap.peek_max().map(|wi| &wi.0)
    }
}

pub mod weighted {
    use crate::agenda::weighted::Weighted;

    /// An adapter for `super::LimitedHeap` that uses the priority given by the
    /// items' implementation of `Weighted`.
    pub struct LimitedHeap<I: Weighted>(super::LimitedHeap<I, I::Weight>)
    where
        I::Weight: Ord;

    impl<I: Weighted> LimitedHeap<I>
    where
        I::Weight: Ord,
    {
        pub fn with_capacity(capacity: usize) -> Self {
            LimitedHeap(super::LimitedHeap::with_capacity(capacity))
        }

        pub fn push(&mut self, element: I) -> Option<I> {
            let priority = element.get_weight();
            self.0.push(element, priority)
        }

        pub fn pop(&mut self) -> Option<I> {
            self.0.pop()
        }

        pub fn clear(&mut self) {
            self.0.clear()
        }

        pub fn len(&self) -> usize {
            self.0.len()
        }

        pub fn peek(&self) -> Option<&I> {
            self.0.peek()
        }
    }

    impl<I: Weighted> IntoIterator for LimitedHeap<I>
    where
        I::Weight: Ord,
    {
        type IntoIter = <super::LimitedHeap<I, I::Weight> as IntoIterator>::IntoIter;
        type Item = I;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }
}

use super::weighted::RemoveWeight;

impl<I, W> IntoIterator for LimitedHeap<I, W>
where
    W: Ord,
{
    type IntoIter = RemoveWeight<I, W, IntoIter<WeightedItem<I, W>>>;
    type Item = I;
    fn into_iter(self) -> Self::IntoIter {
        self.heap.into_iter().into()
    }
}
