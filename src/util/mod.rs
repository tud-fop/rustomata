pub mod agenda;
pub mod partition;
pub mod push_down;
pub mod integerisable;
pub mod parsing;
pub mod tree;

use fnv::{FnvHashMap, FnvHashSet};

/// A `HashMap` with `usize` `Key`s.
/// It uses the `Fnv` hasher to provide fast access and insert
/// functionality with these keys.
pub type IntMap<T> = FnvHashMap<usize, T>;
// A `HashSet` with `usize` `Key`s.
/// It uses the `Fnv` hasher to provide fast access and insert
/// functionality with these keys.
pub type IntSet    = FnvHashSet<usize>;


/// Fills a `Vec` with default entries until it can access it at
/// the specified index to return the mutuable reference.
pub fn vec_entry<T>(v: &mut Vec<T>, i: usize) -> &mut T 
where
    T: Default + Clone
{
    if i >= v.len() {
        let diff = i - v.len() + 1;
        v.extend(vec![Default::default(); diff]);
    }
    v.get_mut(i).unwrap()
}

use time::{PreciseTime, Duration};
pub fn with_time<B, F>(f: F) -> (B, Duration) where F: FnOnce () -> B {
    let t0 = PreciseTime::now();
    let result = f();
    let t1 = PreciseTime::now();
    
    (result, t0.to(t1))
}
