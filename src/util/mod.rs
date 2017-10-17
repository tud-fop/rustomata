pub mod integeriser;
pub mod log_prob;
pub mod parsing;
pub mod equivalence_classes;
pub mod ctf;
pub mod ctf_benchmark;

pub use self::integeriser::*;

pub fn ptr_eq<T>(a: *const T, b: *const T) -> bool { a == b }
