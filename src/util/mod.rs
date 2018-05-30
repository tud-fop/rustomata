pub mod agenda;
pub mod partition;
pub mod push_down;
pub mod integerisable;
pub mod parsing;
pub mod reverse;
pub mod search;
pub mod tree;
pub mod factorizable;

use fnv::{FnvHashMap, FnvHashSet};

/// A `HashMap` with `usize` `Key`s.
/// It uses the `Fnv` hasher to provide fast access and insert
/// functionality with these keys.
pub type IntMap<T> = FnvHashMap<usize, T>;
// A `HashSet` with `usize` `Key`s.
/// It uses the `Fnv` hasher to provide fast access and insert
/// functionality with these keys.
pub type IntSet = FnvHashSet<usize>;


/// Fills a `Vec` with default entries until it can access it at
/// the specified index to return the mutuable reference.
pub fn vec_entry<T>(v: &mut Vec<T>, i: usize) -> &mut T
where
    T: Default + Clone,
{
    if i >= v.len() {
        let diff = i - v.len() + 1;
        v.extend(vec![Default::default(); diff]);
    }
    v.get_mut(i).unwrap()
}

/// Measures the time needed to execute a given command.
use time::{PreciseTime, Duration};
pub fn with_time<B, F>(f: F) -> (B, Duration)
where
    F: FnOnce() -> B,
{
    let t0 = PreciseTime::now();
    let result = f();
    let t1 = PreciseTime::now();

    (result, t0.to(t1))
}


/// Like `Take`, but uses a `Capacity` instead of a `usize`. 
pub enum TakeCapacity<I: Iterator> {
    Inf(I),
    Lim(I, usize)
}

use std::ops::SubAssign;
impl<I: Iterator> Iterator for TakeCapacity<I> {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            &mut TakeCapacity::Inf(ref mut it) => it.next(),
            &mut TakeCapacity::Lim(_, 0) => None,
            &mut TakeCapacity::Lim(ref mut it, ref mut i) => { i.sub_assign(1); it.next() }
        }
    }
}

/// Construct a `CapacityIterator`.
pub fn take_capacity<I: Iterator>(it: I, c: agenda::Capacity) -> TakeCapacity<I> {
    use self::agenda::Capacity::*;
    match c {
        Infinite => TakeCapacity::Inf(it),
        Limit(i) => TakeCapacity::Lim(it, i)
    }
}