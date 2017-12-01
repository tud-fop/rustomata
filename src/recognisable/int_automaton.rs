use std::collections::BinaryHeap;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Mul;

use num_traits::One;

use recognisable::{Automaton, Configuration, Instruction, Item, Recogniser, Transition, TransitionMap};
use integeriser::Integeriser;
use util::agenda::{Agenda, BoundedPriorityQueue};
use util::push_down::Pushdown;

pub trait Integerisable1  // TODO move to util::integerisable
    where Self::I: Integeriser,
{
    type AInt; /// type of the integerised self
    type I;    /// type of the integeriser

    fn integerise(&self, integeriser: &mut Self::I) -> Self::AInt;

    fn un_integerise(&Self::AInt, integeriser: &Self::I) -> Self;
}

pub trait Integerisable2  // TODO move to util::integerisable
    where Self::I1: Integeriser,
          Self::I2: Integeriser,
{
    type AInt; /// type of the integerised self
    type I1;   /// type of the first integeriser
    type I2;   /// type of the second integeriser

    fn integerise(&self, integeriser1: &mut Self::I1, integeriser2: &mut Self::I2) -> Self::AInt;

    fn un_integerise(&Self::AInt, integeriser1: &Self::I1, integeriser2: &Self::I2) -> Self;
}

pub trait IntAutomaton<T, W>: Automaton<T, W>
    where T: Clone + Debug + Eq,
          W: Clone + Copy + Debug + Eq + Mul<Output=W> + One + Ord,
          Self::IKey: Hash + Eq + Clone,
          Self::IInt: Clone + Debug + Eq + Instruction,
          <Self::I as Instruction>::Storage: Clone + Debug + Eq,
          <Self::IInt as Instruction>::Storage: Clone + Debug + Eq,
{
    type IInt; /// integerised instruction
    type IKey; /// key of the integerised configuration

    fn extract_key_int(&Configuration<<Self::IInt as Instruction>::Storage, usize, W>) -> &Self::IKey;

    fn is_terminal_int(&Configuration<<Self::IInt as Instruction>::Storage, usize, W>) -> bool;

    fn item_map(&self, &Item<<Self::IInt as Instruction>::Storage, Self::IInt, usize, W>) -> Item<<Self::I as Instruction>::Storage, Self::I, T, W>;

    fn transitions_int(&self) -> TransitionMap<Self::IKey, Self::IInt, usize, W>;

    fn initial_int(&self) -> <Self::IInt as Instruction>::Storage;

    fn terminal_to_int(&self, &T) -> usize;
}


// recognisers
type ExactRecogniserInt<'a, S, SInt, I, IInt, T, W, K> =
    Recogniser<'a,
               BinaryHeap<Item<SInt, IInt, usize, W>>,
               Configuration<SInt, usize, W>,
               Transition<IInt, usize, W>,
               K,
               Item<S, I, T, W>>;

pub fn recognise<'a, A, T, W>(a: &'a A, word: Vec<T>)
                                 -> Box<ExactRecogniserInt<'a, <A::I as Instruction>::Storage, <A::IInt as Instruction>:: Storage, A::I, A::IInt, T, W, A::IKey>>
    where A: IntAutomaton<T, W>,
          A::I: Clone + Debug + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Debug + Eq,
          <A::IInt as Instruction>::Storage: Clone + Debug + Eq,
          T: Clone + Debug + Eq,
          W: Copy + Debug + One + Ord,
{
    let i = Configuration {
        word: word.iter().map(|t| a.terminal_to_int(t)).collect(),
        storage: a.initial_int(),
        weight: W::one(),
    };

    let mut init_heap = BinaryHeap::new();
    init_heap.enqueue((i, Pushdown::new()));

    Box::new(
        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| A::extract_key_int(c)),
            filtered_rules: a.transitions_int(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(|c| A::is_terminal_int(c)),
            item_map: Box::new(move |i| a.item_map(i)),
        }
    )
}

type BeamRecogniserInt<'a, S, SInt, I, IInt, T, W, K> =
    Recogniser<'a,
               BoundedPriorityQueue<W, Item<SInt, IInt, usize, W>>,
               Configuration<SInt, usize, W>,
               Transition<IInt, usize, W>,
               K,
               Item<S, I, T, W>>;

pub fn recognise_beam<'a, A, T, W>(a: &'a A, beam: usize, word: Vec<T>)
                                 -> Box<BeamRecogniserInt<'a, <A::I as Instruction>::Storage, <A::IInt as Instruction>:: Storage, A::I, A::IInt, T, W, A::IKey>>
    where A: IntAutomaton<T, W>,
          A::I: Clone + Debug + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Debug + Eq,
          <A::IInt as Instruction>::Storage: Clone + Debug + Eq,
          T: Clone + Debug + Eq,
          W: Copy + Debug + One + Ord,
{
    let i = Configuration {
        word: word.iter().map(|t| a.terminal_to_int(t)).collect(),
        storage: a.initial_int(),
        weight: W::one(),
    };

    let mut init_heap = BoundedPriorityQueue::new(beam);
    init_heap.enqueue((i, Pushdown::new()));

    Box::new(
        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| A::extract_key_int(c)),
            filtered_rules: a.transitions_int(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(|c| A::is_terminal_int(c)),
            item_map: Box::new(move |i| a.item_map(i)),
        }
    )
}

