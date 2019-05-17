use std::collections::{hash_map::RandomState, BinaryHeap, HashSet};
use std::hash::{BuildHasher, Hash};
use std::iter::FromIterator;

#[cfg(feature = "fnvtype")]
extern crate fnv;
#[cfg(feature = "fnvtype")]
pub type FnvUniqueHeap<I, W> = UniqueHeap<I, W, fnv::FnvBuildHasher>;

/// This is a priority queue implemented as max-heap. The priority and the
/// elements themselves are separate, i.e. the heap orders first via priority
/// and secondary via the order of elements. It holds each element at most
/// once.
/// # Examples
/// ```
/// use unique_heap::UniqueHeap;
/// let mut heap = vec![("high", 10), ("low", 1), ("medium", 5), ("high", 12)]
///                 .into_iter()
///                 .collect::<UniqueHeap<_, _>>();
/// assert_eq!(heap.peek(), Some(&(10, "high")));
/// assert!(!heap.push("low", 2));
/// assert_eq!(heap.pop(), Some(("high", 10)));
/// assert_eq!(heap.pop(), Some(("medium", 5)));
/// assert_eq!(heap.pop(), Some(("low", 1)));
/// assert_eq!(heap.pop(), None);
/// ```
#[derive(Debug, Clone)]
pub struct UniqueHeap<It, Wt, Bh = RandomState>
where
    Wt: Ord,
    It: Ord + Hash,
    Bh: BuildHasher,
{
    heap: BinaryHeap<(Wt, It)>,
    current_items: HashSet<It, Bh>,
}

impl<It, Wt, Bh> UniqueHeap<It, Wt, Bh>
where
    It: Ord + Hash,
    Wt: Ord,
    Bh: BuildHasher,
{
    /// Pushes an element onto the heap, if it is not present.
    /// This method returns true, if the element was pushed.
    pub fn push(&mut self, item: It, weight: Wt) -> bool
    where
        It: Clone,
    {
        if self.current_items.insert(item.clone()) {
            self.heap.push((weight, item));
            true
        } else {
            false
        }
    }

    /// Pops from the heap.
    pub fn pop(&mut self) -> Option<(It, Wt)> {
        if let Some((weight, item)) = self.heap.pop() {
            self.current_items.remove(&item);
            Some((item, weight))
        } else {
            None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.heap.is_empty()
    }

    pub fn into_sorted_vec(self) -> Vec<(Wt, It)> {
        self.heap.into_sorted_vec()
    }

    pub fn peek(&self) -> Option<&(Wt, It)> {
        self.heap.peek()
    }

    pub fn len(&self) -> usize {
        self.heap.len()
    }
}

impl<I, W, B> Default for UniqueHeap<I, W, B>
where
    B: Default + BuildHasher,
    I: Ord + Hash,
    W: Ord,
{
    fn default() -> Self {
        UniqueHeap {
            heap: BinaryHeap::default(),
            current_items: HashSet::default(),
        }
    }
}

impl<I, W, B> FromIterator<(I, W)> for UniqueHeap<I, W, B>
where
    I: Ord + Hash + Clone,
    W: Ord,
    B: BuildHasher + Default,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (I, W)>,
    {
        let iter = iter.into_iter();
        let n = match iter.size_hint() {
            (_, Some(upper)) => upper,
            (lower, None) => lower,
        };
        let mut heap = BinaryHeap::with_capacity(n);
        let mut current_items = HashSet::with_capacity_and_hasher(n, Default::default());

        for (item, weight) in iter {
            if current_items.insert(item.clone()) {
                heap.push((weight, item));
            }
        }

        UniqueHeap {
            heap,
            current_items,
        }
    }
}

impl<I, W, B> From<Vec<(W, I)>> for UniqueHeap<I, W, B>
where
    I: Ord + Hash + Clone,
    W: Ord,
    B: BuildHasher + Default,
{
    fn from(vec: Vec<(W, I)>) -> Self {
        let current_items = vec.iter().map(|(_, i)| i).cloned().collect();
        UniqueHeap {
            heap: vec.into(),
            current_items,
        }
    }
}

impl<I, W, B> IntoIterator for UniqueHeap<I, W, B>
where
    I: Ord + Hash + Clone,
    W: Ord,
    B: BuildHasher,
{
    type IntoIter = ::std::collections::binary_heap::IntoIter<(W, I)>;
    type Item = (W, I);

    fn into_iter(self) -> Self::IntoIter {
        self.heap.into_iter()
    }
}

impl<'a, I, W, B> IntoIterator for &'a UniqueHeap<I, W, B>
where
    I: Ord + Hash + Clone + 'a,
    W: Ord + 'a,
    B: BuildHasher + 'a,
{
    type IntoIter = ::std::collections::binary_heap::Iter<'a, (W, I)>;
    type Item = &'a (W, I);

    fn into_iter(self) -> Self::IntoIter {
        self.heap.iter()
    }
}
