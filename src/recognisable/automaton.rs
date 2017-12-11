use std::collections::{BinaryHeap, HashMap};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Mul;
use std::rc::Rc;

use num_traits::One;

// use coarse_to_fine::{run_weight, run_word};
use recognisable::{Configuration, Instruction, Item, Recogniser, Transition};
use util::agenda::{Agenda, BoundedPriorityQueue};
use util::push_down::Pushdown;

// map from key to transition
pub type TransitionMap<K, I, T, W> = HashMap<K, BinaryHeap<Transition<I, T, W>>>;

// TODO: slim down interface
pub trait Automaton<T, W>
    where T: Clone + Debug + Eq,
          W: Clone + Copy + Debug + Eq + Mul<Output=W> + One + Ord,
          Self::IKey: Hash + Eq + Clone,
          Self::I: Clone + Debug + Eq + Instruction,
          Self::IInt: Clone + Debug + Eq + Instruction,
          <Self::I as Instruction>::Storage: Clone + Debug + Eq,
          <Self::IInt as Instruction>::Storage: Clone + Debug + Eq,
{
    type I;    /// instructions
    type IInt; /// internal representation of instructions
    type TInt; /// internal representation of terminal symbols
    type IKey; /// key for the configurations

    fn extract_key_int(&Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>) -> &Self::IKey;

    fn is_terminal_int(&Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>) -> bool;

    fn item_map(&self, &Item<<Self::IInt as Instruction>::Storage, Self::IInt, Self::TInt, W>)
                -> Item<<Self::I as Instruction>::Storage, Self::I, T, W>;

    fn transitions<'a>(&'a self) -> Box<Iterator<Item=Transition<Self::I, T, W>> + 'a>;

    fn transition_map(&self) -> Rc<TransitionMap<Self::IKey, Self::IInt, Self::TInt, W>>;

    fn initial(&self) -> <Self::IInt as Instruction>::Storage;

    fn terminal_to_int(&self, &T) -> Self::TInt;

    /*
    // TODO: remove
    fn check_run(&self, run: &[Transition<Self::IInt, T, W>])
                 -> Option<VecItem<<Self::IInt as Instruction>::Storage, Self::IInt, T, W>>
    {
        let heap = self.check(self.initial().clone(), run);
        if heap.is_empty(){
            return None;
        }
        let c = Configuration {
            word: run_word(run),
            storage: heap[0].clone(),
            weight: run_weight(run),
        };
        Some((c, run.to_owned()))
    }

    // TODO: remove
    // note: gives back the first configuration it finds
    fn check<'b>(&'b self, storage: <Self::IInt as Instruction>::Storage, run: &[Transition<Self::IInt, T, W>])
                 -> Vec<<Self::IInt as Instruction>::Storage>
    {
        let mut storage_heap = Vec::new();
        storage_heap.push(storage);
        for t in run {
            let mut new_storage_heap = Vec::new();
            for s in storage_heap{
                for s1 in t.instruction.apply(s) {
                    new_storage_heap.push(s1);
                }
            }
            storage_heap = new_storage_heap;
        }
        storage_heap
    }
     */
}


pub fn recognise<'a, A, T, W>(a: &'a A, word: Vec<T>)
                              -> Box<Iterator<Item=Item<<A::I as Instruction>::Storage, A::I, T, W>> + 'a>
    where A: Automaton<T, W>,
          A::I: Clone + Debug + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Debug + Eq,
          A::IInt: 'a,
          A::IKey: 'a,
          <A::IInt as Instruction>::Storage: Clone + Debug + Eq + Ord,
          T: Clone + Debug + Eq + Ord + 'a,
          A::TInt: Clone + Debug + Eq + Ord,
          W: Copy + Debug + One + Ord + 'a,
{
    let i = Configuration {
        word: word.iter().map(|t| a.terminal_to_int(t)).collect(),
        storage: a.initial(),
        weight: W::one(),
    };

    let mut init_heap = BinaryHeap::new();
    init_heap.enqueue((i, Pushdown::new()));

    Box::new(
        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| A::extract_key_int(c)),
            filtered_rules: a.transition_map(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(|c| A::is_terminal_int(c)),
            item_map: Box::new(move |i| a.item_map(&i)),
        }
    )
}


pub fn recognise_beam<'a, A, T, W>(a: &'a A, beam: usize, word: Vec<T>)
                               -> Box<Iterator<Item=Item<<A::I as Instruction>::Storage, A::I, T, W>> + 'a>
    where A: Automaton<T, W>,
          A::I: Clone + Debug + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Debug + Eq,
          A::IInt: 'a,
          A::IKey: 'a,
          <A::IInt as Instruction>::Storage: Clone + Debug + Eq + Ord,
          T: Clone + Debug + Eq + Ord + 'a,
          A::TInt: Clone + Debug + Eq + Ord,
          W: Copy + Debug + One + Ord + 'a,
{
    let i = Configuration {
        word: word.iter().map(|t| a.terminal_to_int(t)).collect(),
        storage: a.initial(),
        weight: W::one(),
    };

    let mut init_heap = BoundedPriorityQueue::new(beam);
    init_heap.enqueue((i, Pushdown::new()));

    Box::new(
        Recogniser {
            agenda: init_heap,
            configuration_characteristic: Box::new(|c| A::extract_key_int(c)),
            filtered_rules: a.transition_map(),
            apply: Box::new(|c, r| r.apply(c)),
            accepting: Box::new(|c| A::is_terminal_int(c)),
            item_map: Box::new(move |i| a.item_map(&i)),
        }
    )
}

