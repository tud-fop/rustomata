use std::collections::BinaryHeap;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use std::str::FromStr;
use num_traits::{One};
use std::ops::{Mul};

use integeriser::HashIntegeriser;

use push_down_automaton::*;
use automata::*;

use util::agenda::{Agenda, BoundedPriorityQueue};
use util::push_down::Pushdown;

use push_down_automaton::PushDown;

mod push_down;
mod tree_stack;
mod from_pmcfg;
mod approximation;

pub use self::push_down::*;
pub use self::tree_stack::*;
pub use self::approximation::*;

type IntConfiguration<S, W> = Configuration<S, usize, W>;
type IntTransition<I, W> = Transition<I, usize, W>;
type IntItemTuple<S, I, W> = (IntConfiguration<S, W>, Pushdown<IntTransition<I, W>>);

// kinds of recognisers
type ExactIntRecogniser<'a, S, I, A, T, W> = IntRecogniser<'a, BinaryHeap<IntItemTuple<S, I, W>>, I, A, T, W>;
type BeamIntRecogniser<'a, S, I, A, T, W> = IntRecogniser<'a, BoundedPriorityQueue<W, IntItemTuple<S, I, W>>, I, A, T, W>;

/// Integerised Version of `Automaton`.
pub trait IntegerisedAutomaton<I, A, T, W>
    where I: Clone + Debug + Eq + Instruction,
          A: Clone + Debug + Eq + Hash,
          T: Clone + Debug + Eq + Hash,
          W: Clone + Debug + Mul<Output=W> + One + Ord,
          Self::Key: Hash + Eq + Clone,
          I::Storage: Clone + Debug + Eq,
{
    type Key;

    fn recognise(&self, word: Vec<T>) -> ExactIntRecogniser<I::Storage, I, A, T, W>;

    fn recognise_beam_search(&self, beam_width: usize, word: Vec<T>) -> BeamIntRecogniser<I::Storage, I, A, T, W>;

    fn check_run<'a>(&'a self, run: &[Transition<I, usize, W>]) -> Option<IntItem<'a, I, A, T, W>>;

    fn int_word(&self, word: Vec<T>)-> Vec<usize>;
}

/// Trait that specifies whether a structure can be integerised using a `Ìntegeriser<A>`
pub trait Integerisable<S, A: Hash + Eq>{
    fn integerise(&self, inter: &mut HashIntegeriser<A>)-> S;

    fn translate(s: S, inter: &HashIntegeriser<A>)-> Self;
}

/// Trait that specifies whether a Structure can be integerised with two `Ìntegeriser`, `Ìntegeriser<A>` and `Ìntegeriser<B>`
pub trait IntegerisableM<S, A: Hash + Eq, B: Hash + Eq>{
    fn integerise(&self, inter1: &mut HashIntegeriser<A>, inter2: &mut HashIntegeriser<B>)-> S;

    fn translate(s: S, inter1: &HashIntegeriser<A>, inter2: &HashIntegeriser<B>)-> Self;
}

/// Integerised version of `Recogniser`. Creates `ÌntItems` as a result
pub struct IntRecogniser<'a, A, I, N, T, W>
    where A: Agenda<Item=IntItemTuple<I::Storage, I, W>>,
          I: Clone + Debug + Eq + Instruction,
          N: 'a + Clone + Debug + Hash + Eq,
          T: 'a + Clone + Debug + Hash + Eq,
          W: Ord + Clone + Debug,
          I::Storage: Eq,
{
    pub term_integeriser: &'a HashIntegeriser<T>,
    pub nterm_integeriser: &'a HashIntegeriser<N>,
    pub recog: Recogniser<'a, A, IntConfiguration<I::Storage, W>, IntTransition<I, W>, usize>,
}

impl<'a, A, I, N, T, W> Iterator for IntRecogniser<'a, A, I, N, T, W>
    where A: Agenda<Item=IntItemTuple<I::Storage, I, W>>,
          I: Clone + Debug + Eq + Instruction,
          N: 'a + Ord + Eq + Debug + Clone + Hash,
          T: 'a + Clone + Debug + Eq + Hash,
          W: Ord + Clone + Debug,
          I::Storage: Clone + Debug + Eq,
{
    type Item = IntItem<'a, I, N, T, W>;

    fn next(&mut self) -> Option<IntItem<'a, I, N, T, W>> {
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
pub struct IntItem<'a, I, N, T, W>
    where I: Instruction,
          N: 'a + Eq + Hash,
          T: 'a + Eq + Hash,
{
    pub configuration: IntConfiguration<I::Storage, W>,
    pub run: Pushdown<IntTransition<I, W>>,
    pub term_integeriser: &'a HashIntegeriser<T>,
    pub nterm_integeriser: &'a HashIntegeriser<N>,
}

impl<'a, I, N, T, W> PartialEq for IntItem<'a, I, N, T, W>
    where I: Instruction + PartialEq,
          I::Storage: PartialEq,
          N: 'a + Eq + Hash,
          T: 'a + Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.configuration == other.configuration && self.run == other.run
    }
}

impl<'a, I, N, T, W> Eq for IntItem<'a, I, N, T, W>
    where I: Eq + Instruction,
          I::Storage: Eq,
          N: 'a + Eq + Hash,
          T: 'a + Eq + Hash,
{}

impl<'a, I, N, T, W> PartialOrd for IntItem<'a, I, N, T, W>
    where I: Instruction + PartialEq,
          I::Storage: Eq,
          W: Eq + PartialOrd,
          N: 'a + Eq + Hash,
          T: 'a + Eq + Hash,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>{
        self.configuration.partial_cmp(&other.configuration)
    }
}

impl<'a, I, N, T, W> Ord for IntItem<'a, I, N, T, W>
    where I: Eq + Instruction,
          I::Storage: Eq,
          W: Eq + Ord,
          N: 'a + Eq + Hash,
          T: 'a + Eq + Hash,
{
    fn cmp(&self, other: &Self) -> Ordering{
        self.configuration.cmp(&other.configuration)
    }
}
