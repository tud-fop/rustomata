pub mod parsing;
pub mod agenda;
pub mod partition;
pub mod push_down;
pub mod integerisable;

use fnv::FnvHashMap;

/// A `HashMap` with `usize` `Key`s.
/// It uses the `Fnv` hasher to provide fast access and insert
/// functionality with these keys.
pub type IntMap<T> = FnvHashMap<usize, T>;


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