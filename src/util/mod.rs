pub mod parsing;
pub mod equivalence_classes;
pub mod ctf;
pub mod ctf_benchmark;
pub mod bounded_priority_queue;

pub fn ptr_eq<T>(a: *const T, b: *const T) -> bool { a == b }
