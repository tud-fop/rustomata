pub mod partition;
pub mod push_down;
pub mod integerisable;
pub mod parsing;
pub mod reverse;
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