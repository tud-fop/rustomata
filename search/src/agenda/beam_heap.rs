use std::collections::{BinaryHeap, binary_heap::IntoIter};
use std::ops::Mul;

use super::weighted::WeightedItem;

/// A heap that only inserts elements if the associated priority is greater
/// than the highest priority in the `BeamHeap` multiplied by a given constant
/// factor.
pub struct BeamHeap<I, W>
where
    W: Ord
{
    heap: BinaryHeap<WeightedItem<I, W>>,
    beta: W
}

impl<I, W> BeamHeap<I, W>
where
    W: Ord
{
    pub fn new(beta: W) -> Self {
        BeamHeap {
            heap: BinaryHeap::new(),
            beta
        }
    }

    pub fn push(&mut self, element: I, priority: W) -> bool
    where
        W: Clone + Mul<Output=W>
    {
        if self.heap.is_empty() || priority >= self.heap.peek().unwrap().1.clone() * self.beta.clone() {
            self.heap.push(WeightedItem(element, priority));
            true
        } else {
            false
        }
    }

    pub fn peek(&self) -> Option<&I> {
        self.heap.peek().map(|wi| &wi.0)
    }

    pub fn pop(&mut self) -> Option<I> {
        self.heap.pop().map(|wi| wi.0)
    }

    pub fn len(&self) -> usize {
        self.heap.len()
    }

    pub fn clear(&mut self) {
        self.heap.clear()
    }
}

pub mod weighted {
    use crate::agenda::weighted::Weighted;
    use std::ops::Mul;
    pub struct BeamHeap<I: Weighted>(super::BeamHeap<I, I::Weight>)
        where I::Weight: Ord;
    
    /// An adapter for `super::BeamHeap` that uses priorities of items given by
    /// the implementation of `Weighted`.
    impl<I: Weighted> BeamHeap<I>
    where
        I::Weight: Ord
    {
        pub fn new(beta: I::Weight) -> Self {
            BeamHeap(super::BeamHeap::new(beta))
        }

        pub fn push(&mut self, element: I) -> bool 
        where
            I::Weight: Clone + Mul<Output=I::Weight>
        {
            let priority = element.get_weight();
            self.0.push(element, priority)
        }

        pub fn pop(&mut self) -> Option<I> {
            self.0.pop()
        }

        pub fn clear(&mut self) {
            self.0.clear()
        }

        pub fn len(&mut self) -> usize {
            self.0.len()
        }
    }


    impl<I: Weighted> IntoIterator for BeamHeap<I>
    where
        I::Weight: Ord
    {
        type IntoIter = <super::BeamHeap<I, I::Weight> as IntoIterator>::IntoIter;
        type Item = I;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }
}

impl<I, W> Clone for BeamHeap<I, W>
where
    I: Clone,
    W: Clone + Ord
{
    fn clone(&self) -> Self {
        BeamHeap {
            heap: self.heap.clone(),
            beta: self.beta.clone()
        }
    }
}

use super::weighted::RemoveWeight;

impl<I, W> IntoIterator for BeamHeap<I, W>
where
    W: Ord
{
    type IntoIter = RemoveWeight<I, W, IntoIter<WeightedItem<I, W>>>;
    type Item = I;
    fn into_iter(self) -> Self::IntoIter {
        self.heap.into_iter().into()
    }
}