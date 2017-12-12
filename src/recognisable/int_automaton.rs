use std::collections::{BinaryHeap, BTreeSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Mul;
use std::rc::Rc;

use num_traits::One;

use recognisable::{Configuration, Instruction, Item, Recogniser, Transition};
use recognisable::automaton::{Automaton, TransitionMap};
use util::agenda::{Agenda, BoundedPriorityQueue, Weighted};
use util::push_down::Pushdown;


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

    fn item_map(&self, &Item<<Self::IInt as Instruction>::Storage, Self::IInt, usize, W>)
                -> Item<<Self::I as Instruction>::Storage, Self::I, T, W>;

    fn transitions_int(&self) -> Rc<TransitionMap<Self::IKey, Self::IInt, usize, W>>;

    fn initial_int(&self) -> <Self::IInt as Instruction>::Storage;

    fn terminal_to_int(&self, &T) -> usize;
}


pub fn recognise<'a, A, T, W>(a: &'a A, word: Vec<T>)
                              -> Box<Iterator<Item=Item<<A::I as Instruction>::Storage, A::I, T, W>> + 'a>
    where A: IntAutomaton<T, W>,
          A::I: Clone + Debug + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Debug + Eq,
          A::IInt: 'a,
          A::IKey: 'a,
          <A::IInt as Instruction>::Storage: Clone + Debug + Eq + Ord,
          T: Clone + Debug + Eq + Ord + 'a,
          W: Copy + Debug + One + Ord + 'a,
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
            already_found: None,
            item_map: Box::new(move |i| a.item_map(&i))
        }
    )
}


pub fn recognise_beam<'a, A, T, W>(a: &'a A, beam: usize, word: Vec<T>)
                               -> Box<Iterator<Item=Item<<A::I as Instruction>::Storage, A::I, T, W>> + 'a>
    where A: IntAutomaton<T, W>,
          A::I: Clone + Debug + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Debug + Eq,
          A::IInt: 'a,
          A::IKey: 'a,
          <A::IInt as Instruction>::Storage: Clone + Debug + Eq + Ord,
          T: Clone + Debug + Eq + Ord + 'a,
          W: Copy + Debug + One + Ord + 'a,
{
    let i = Configuration {
        word: word.iter().map(|t| a.terminal_to_int(t)).collect(),
        storage: a.initial_int(),
        weight: W::one(),
    };

    let mut init_heap = BoundedPriorityQueue::new(
        beam, 
        Box::new(
            | cp: &(Configuration<<A::IInt as Instruction>::Storage, usize, W>, Pushdown<Transition<A::IInt, usize, W>>) | {
                cp.get_weight()
            }
        )
    );
    init_heap.enqueue((i, Pushdown::new()));

    Box::new(
        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| A::extract_key_int(c)),
            filtered_rules: a.transitions_int(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(|c| A::is_terminal_int(c)),
            already_found: None,
            item_map: Box::new(move |i| a.item_map(&i))
        }
    )
}

