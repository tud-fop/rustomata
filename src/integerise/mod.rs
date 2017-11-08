use std::marker::PhantomData;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use std::str::FromStr;
use num_traits::{One};
use std::ops::{Mul};

use integeriser::HashIntegeriser;

use push_down::*;
use automata::*;
use util::*;

use push_down::PushDown;

mod push_down;
mod tree_stack;
mod from_pmcfg;
mod approximation;

pub use self::push_down::*;
pub use self::tree_stack::*;
pub use self::approximation::*;

/// Integerised Version of `Automaton`.
pub trait IntegerisedAutomaton<S: Clone + Debug + Eq,
                               I: Clone + Debug + Eq + Instruction<S>,
                               T: Clone + Debug + Eq + Hash,
                               A: Clone + Debug + Eq + Hash,
                               W: Clone + Debug + Mul<Output=W> + One + Ord>
    where Self::Key: Hash + Eq + Clone {

    type Key;

    fn recognise<'a>(&'a self, word: Vec<T>) -> IntRecogniser<'a, S, I, T, A, W>;

    fn check_run<'a>(&'a self, run: &Vec<Transition<S, I, usize, W>>) -> Option<IntItem<'a, S, I, T, A, W>>;

    fn int_word(&self, word: Vec<T>)-> Vec<usize>;
}

/// Trait that specifies whether a structure can be integerised using a `Ìntegeriser<A>`
pub trait Integerisable<S, A: Hash + Eq>{
    fn integerise(&self, inter: &mut HashIntegeriser<A>)-> S;

    fn translate(s: S, inter: &HashIntegeriser<A>)-> Self;
}

///Trait that specifies whether a Structure can be integerised with two `Ìntegeriser`, `Ìntegeriser<A>` and `Ìntegeriser<B>`
pub trait IntegerisableM<S, A: Hash + Eq, B: Hash + Eq>{
    fn integerise(&self, inter1: &mut HashIntegeriser<A>, inter2: &mut HashIntegeriser<B>)-> S;

    fn translate(s: S, inter1: &HashIntegeriser<A>, inter2: &HashIntegeriser<B>)-> Self;
}

///Integerised version of `Recogniser`. Creates `ÌntItems` as a result
pub struct IntRecogniser<'a,
         S: Clone + Debug + Eq,
         I: Clone + Debug + Eq + Instruction<S>,
         T: 'a + Clone + Debug + Hash + Eq,
         A: 'a + Clone + Debug + Hash + Eq,
         W: Ord + Clone + Debug>{
    pub term_integeriser: &'a HashIntegeriser<T>,
    pub nterm_integeriser: &'a HashIntegeriser<A>,
    pub recog: Recogniser<'a, Configuration<S, usize, W>, Transition<S, I, usize, W>, usize>,
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

/// Structure holding the results of a `IntRecogniser`. Besides the created `run` and `configuration` also holds pointer to the two `Integeriser` used to create the initial `IntegerisedAutomaton`
#[derive(Clone, Debug)]
pub struct IntItem<'a, S: Clone + Eq,
                   I: Clone + Eq + Instruction<S>,
                   T: 'a + Clone+ Eq + Hash,
                   A: 'a + Clone + Hash + Eq,
                   W: Ord + Clone>{
    pub configuration: Configuration<S, usize, W>,
    pub run: Pushdown<Transition<S, I, usize, W>>,
    pub term_integeriser: &'a HashIntegeriser<T>,
    pub nterm_integeriser: &'a HashIntegeriser<A>,
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
