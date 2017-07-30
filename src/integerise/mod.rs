use std::marker::PhantomData;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use std::str::FromStr;
use num_traits::{One};
use std::ops::{Mul};

use push_down::*;

pub mod push_down;
pub mod tree_stack;

pub use self::push_down::*;
pub use self::tree_stack::*;

pub trait IntegerisedAutomaton<S: Clone + Debug + Eq,
                    I: Clone + Debug + Eq + Instruction<S>,
                    T: Clone + Debug + Eq + Hash,
                    A: Clone + Debug + Eq + Hash,
                    W: One + Mul<Output = W> + Clone + Copy + Debug + Eq + Ord>
    where Self::Key: Hash + Eq + Clone {

    type Key;

    fn recognise<'a>(&'a self, word: Vec<T>) -> IntRecogniser<'a, S, I, T, A, W>;

    fn check_run<'a>(&'a self, run: &Vec<Transition<S, I, u64, W>>, word: Vec<T>) -> Option<IntItem<'a, S, I, T, A, W>>;
}

pub trait IntApproximation<T1, T2, O> {
    fn approximation(&self, &T1) -> Result<(O, T2), String>;
}

pub trait Integerisable<S, A: Hash + Eq>{
    fn integerise(&self, inter: &mut Integeriser<A>)-> S;

    fn translate(s: S, inter: &Integeriser<A>)-> Self;
}

pub trait IntegerisableM<S, A: Hash + Eq, B: Hash + Eq>{
    fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)-> S;

    fn translate(s: S, inter1: &Integeriser<A>, inter2: &Integeriser<B>)-> Self;
}

pub struct IntRecogniser<'a,
         S: Clone + Debug + Eq,
         I: Clone + Debug + Eq + Instruction<S>,
         T: 'a + Clone + Debug + Hash + Eq,
         A: 'a + Clone + Debug + Hash + Eq,
         W: Ord + Clone + Debug>{
    pub term_integeriser: &'a Integeriser<T>,
    pub nterm_integeriser: &'a Integeriser<A>,
    pub recog: Recogniser<'a, Configuration<S, u64, W>, Transition<S, I, u64, W>, u64>,
}

impl<'a, S: Clone + Debug + Eq,
         I: Clone + Debug + Eq + Instruction<S>,
         T: 'a + Clone + Debug + Eq + Hash,
         A: 'a + Ord + Eq + Debug + Clone + Hash,
         W: Ord + Clone + Debug> Iterator for IntRecogniser<'a, S, I, T, A, W> {
    type Item = IntItem<'a, S, I, T, A, W>;

    fn next(&mut self) -> Option<IntItem<'a, S, I, T, A, W>> {
        match self.recog.next(){
            Some((c, run)) =>{
                Some(IntItem{
                    configuration: c,
                    run: run,
                    term_integeriser: self.term_integeriser,
                    nterm_integeriser: self.nterm_integeriser,
                })
            },
            None => None
        }
    }
}

#[derive(Clone, Debug)]
pub struct IntItem<'a, S: Clone + Eq,
                   I: Clone + Eq + Instruction<S>,
                   T: 'a + Clone+ Eq + Hash,
                   A: 'a + Clone + Hash + Eq,
                   W: Ord + Clone>{
    pub configuration: Configuration<S, u64, W>,
    pub run: Vec<Transition<S, I, u64, W>>,
    pub term_integeriser: &'a Integeriser<T>,
    pub nterm_integeriser: &'a Integeriser<A>,
}

impl<'a,S: Clone + Eq + Debug,
                   I: Debug + Clone + Eq + Instruction<S>,
                   T: 'a + Clone+ Eq + Debug + Hash,
                   A: 'a + Clone + Debug + Hash + Eq,
                   W: Ord + Clone + Debug> PartialEq for IntItem<'a, S, I, T, A, W>{
    fn eq(&self, other: &Self) -> bool{
        self.configuration == other.configuration && self.run == other.run
    }
}

impl<'a,S: Clone + Eq + Debug,
                   I: Debug + Clone + Eq + Instruction<S>,
                   T: 'a + Clone+ Eq + Debug + Hash,
                   A: 'a + Clone + Debug + Hash + Eq,
                   W: Ord + Clone + Debug> Eq for IntItem<'a, S, I, T, A, W>{ }

impl<'a,S: Clone + Eq + Debug,
                   I: Debug + Clone + Eq + Instruction<S>,
                   T: 'a + Clone+ Eq + Debug + Hash,
                   A: 'a + Clone + Debug + Hash + Eq,
                   W: Ord + Clone + Debug> PartialOrd for IntItem<'a, S, I, T, A, W>{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>{
        self.configuration.partial_cmp(&other.configuration)
    }
}

impl<'a,S: Clone + Eq + Debug,
                   I: Debug + Clone + Eq + Instruction<S>,
                   T: 'a + Clone+ Eq + Debug + Hash,
                   A: 'a + Clone + Debug + Hash + Eq,
                   W: Ord + Clone + Debug> Ord for IntItem<'a, S, I, T, A, W>{
    fn cmp(&self, other: &Self) -> Ordering{
        self.configuration.cmp(&other.configuration)
    }
}
