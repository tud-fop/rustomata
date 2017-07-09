use std::marker::PhantomData;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use std::str::FromStr;
use num_traits::{One};
use std::ops::{Mul};

use util::integeriser::*;
use push_down::*;

pub mod push_down;
pub mod tree_stack;

pub use self::push_down::*;
pub use self::tree_stack::*;

pub trait IntegerisedAutomaton<S: Clone + Debug + Eq,
                    I: Clone + Debug + Eq + Instruction<S>,
                    T: Clone + Debug + Eq,
                    A: Clone + Debug + Eq,
                    W: One + Mul<Output = W> + Clone + Copy + Debug + Eq + Ord>
    where Self::Key: Hash + Eq + Clone {

    type Key;

    fn recognise<'a>(&'a self, word: Vec<T>) -> IntRecogniser<'a, S, I, T, A, W>;
}

pub trait Integerisable<S, A>{
    fn integerise(&self, inter: &mut Integeriser<A>)-> S;

    fn translate(s: S, inter: &mut Integeriser<A>)-> Self;
}

pub trait IntegerisableM<S, A, B>{
    fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)-> S;

    fn translate(s: S, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)-> Self;
}

pub struct IntRecogniser<'a,
         S: Clone + Debug + Eq,
         I: Clone + Debug + Eq + Instruction<S>,
         T: Clone + Debug + Eq,
         A: Clone + Debug,
         W: Ord + Clone + Debug>{
    pub term_integeriser: Integeriser<T>,
    pub nterm_integeriser: Integeriser<A>,
    pub recog: Recogniser<'a, Configuration<S, u64, W>, Transition<S, I, u64, W>, u64>,
}
