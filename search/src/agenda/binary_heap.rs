use std::collections::{binary_heap::IntoIter, BinaryHeap};

pub mod weighted {
    use crate::agenda::weighted::{RemoveWeight, Weighted, WeightedItem};
    use std::iter::FromIterator;

    /// An adapter for `BinaryHeap` that orders elements via the weight
    /// provided by the implementation of `Weighted`.
    pub struct BinaryHeap<I: Weighted>(super::BinaryHeap<WeightedItem<I, I::Weight>>)
    where
        I::Weight: Ord;

    impl<I: Weighted> BinaryHeap<I>
    where
        I::Weight: Ord,
    {
        pub fn new() -> Self {
            BinaryHeap(super::BinaryHeap::new())
        }

        pub fn len(&self) -> usize {
            self.0.len()
        }

        pub fn clear(&mut self) {
            self.0.clear()
        }

        pub fn push(&mut self, element: I) {
            let priority = element.get_weight();
            self.0.push(WeightedItem(element, priority));
        }

        pub fn pop(&mut self) -> Option<I> {
            self.0.pop().map(|wi| wi.0)
        }

        pub fn peek(&self) -> Option<&I> {
            self.0.peek().map(|wi| &wi.0)
        }
    }

    impl<I: Weighted> FromIterator<I> for BinaryHeap<I>
    where
        I::Weight: Ord,
    {
        fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
            BinaryHeap(
                iter.into_iter()
                    .map(|item| {
                        let priority = item.get_weight();
                        WeightedItem(item, priority)
                    })
                    .collect(),
            )
        }
    }

    impl<I: Weighted> IntoIterator for BinaryHeap<I>
    where
        I::Weight: Ord,
    {
        type IntoIter = RemoveWeight<I, I::Weight, super::IntoIter<WeightedItem<I, I::Weight>>>;
        type Item = I;
        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter().into()
        }
    }
}
