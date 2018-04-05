use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::iter::empty;
use std::ops::MulAssign;
use std::rc::Rc;

use num_traits::One;

use recognisable::{Configuration, Instruction, Item, Recogniser, Transition};
use util::agenda::{Agenda, PriorityQueue, Capacity};
use util::push_down::Pushdown;

// map from key to transition
pub type TransitionMap<K, I, T, W> = HashMap<K, BinaryHeap<Transition<I, T, W>>>;

/// An `Automaton` is something that has `transitions`, an `initial` storage configuration, and a predicate characterising terminal configurations `is_terminal`.
///
/// * `T` is the type of the terminal symbols that are read by the automaton.
/// * `W` is the type of the weights that are used by the automaton.
///
/// To allow for optimisations (e.g. integerisation), `Self::I` and `T` are internally represented by `Self::IInt` and `Self::TInt`, respectively.
///
/// * The internal representation should be used for computation-intensive tasks.
/// * The outer representation should be used for pretty-printing etc.
pub trait Automaton<T, W>
    where Self::Key: Eq + Hash,
          Self::I: Clone + Instruction,
          Self::IInt: Clone + Eq + Instruction,
          T: Clone,
          W: Clone + MulAssign + One,
{
    /// A key to match `Configuration`s to probably applicable `Transitions`.
    type Key;

    /// The `Instruction`s of this `Automaton`.
    type I;

    /// The internal representation of the `Instructions`.
    type IInt;

    /// The internal representation of the terminal symbols
    type TInt;


    /// Builds an `Automaton` from transitions and an initial storage configuration.
    fn from_transitions<It>(transitions: It, initial: <Self::I as Instruction>::Storage)
                            -> Self
        where It: IntoIterator<Item=Transition<Self::I, T, W>>;

    /// Returns a boxed `Iterator` over the `Transitions` of this `Automaton`.
    fn transitions<'a>(&'a self)
                       -> Box<Iterator<Item=Transition<Self::I, T, W>> + 'a>;


    /// Returns the initial storage configuration.
    fn initial(&self) -> <Self::I as Instruction>::Storage;

    /// Maps items from the internal representation to the desired output.
    fn item_map(&self,
                i: &Item<<Self::IInt as Instruction>::Storage, Self::IInt, Self::TInt, W>)
                -> Item<<Self::I as Instruction>::Storage, Self::I, T, W>;

    /// Translates a terminal symbol to its internal representation.
    fn terminal_to_int(&self, t: &T)
                       -> Option<Self::TInt>;


    /// Returns the `Self::Key` for the given `Configuration` (in its internal representation).
    fn extract_key(c: &Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>)
                   -> &Self::Key;

    /// Returns whether the given `Configuration`  (in its internal representation) is terminal,
    /// i.e. if it is a `Configuration` in which the `Automaton` may stop and accept.
    fn is_terminal(&self, c: &Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>)
                   -> bool;

    /// Returns a `Map` from `Self::Key` to the matching `Transition`s (in their internal representation).
    fn transition_map(&self)
                      -> Rc<TransitionMap<Self::Key, Self::IInt, Self::TInt, W>>;

    /// Returns the initial storage configuration (in its internal representation).
    fn initial_int(&self)
                   -> <Self::IInt as Instruction>::Storage;

    // TODO documentation, implement function check_run_int(… Self::IInt …)
    fn check_run(&self, run: Pushdown<Transition<Self::I, T, W>>)
                 -> Vec<Item<<Self::I as Instruction>::Storage, Self::I, T, W>>
    {
        let mut result = Vec::new();
        let mut weight = W::one();
        result.push(self.initial());
        for t in run.iter() {
            weight *= t.weight.clone();
            let mut new_storages = Vec::new();
            for s in result {
                new_storages.append(&mut t.instruction.apply(s))
            }
            result = new_storages;
        }
        result
            .into_iter()
            .map(|s| (Configuration { word: Vec::new(), storage: s, weight: weight.clone() }, run.clone()))
            .collect()
    }

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
          A::I: Clone + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Eq,
          A::IInt: 'a + Ord,
          A::Key: 'a,
          <A::IInt as Instruction>::Storage: Clone + Eq + Ord,
          T: Clone + Eq + Ord + 'a,
          A::TInt: Clone + Eq + Ord,
          W: Copy + MulAssign + One + Ord + 'a,
{
    if let Some(the_word) = word.iter().map(|t| a.terminal_to_int(t)).collect() {
        let i = Configuration {
            word: the_word,
            storage: a.initial_int(),
            weight: W::one(),
        };

        let mut init_heap = BinaryHeap::new();
        init_heap.enqueue((i, Pushdown::new()));

        Box::new(
            Recogniser {
                agenda: init_heap,
                configuration_characteristic: Box::new(|c| A::extract_key(c)),
                filtered_rules: a.transition_map(),
                apply: Box::new(|c, r| r.apply(c)),
                accepting: Box::new(move |c| a.is_terminal(c)),
                item_map: Box::new(move |i| a.item_map(&i)),
                already_found: None
            }
        )
    } else {
        Box::new(empty())
    }
}


pub fn recognise_beam<'a, A, T, W>(a: &'a A, beam: usize, word: Vec<T>)
                               -> Box<Iterator<Item=Item<<A::I as Instruction>::Storage, A::I, T, W>> + 'a>
    where A: Automaton<T, W>,
          A::I: Clone + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Eq,
          A::IInt: 'a + Ord,
          A::Key: 'a,
          <A::IInt as Instruction>::Storage: Clone + Eq + Ord,
          T: Clone + Eq + Ord + 'a,
          A::TInt: Clone + Eq + Ord,
          W: Copy + MulAssign + One + Ord + 'a,
{
    if let Some(the_word) = word.iter().map(|t| a.terminal_to_int(t)).collect() {
        let i = Configuration {
            word: the_word,
            storage: a.initial_int(),
            weight: W::one(),
        };
        let mut init_heap = PriorityQueue::new(
            Capacity::Limit(beam), 
            Box::new(
                | cp: &(Configuration<<A::IInt as Instruction>::Storage, A::TInt, W>, Pushdown<Transition<A::IInt, A::TInt, W>>) | { 
                    let &Configuration{ ref weight, .. } = &cp.0;
                    *weight
                }
            )
        );
        init_heap.enqueue((i, Pushdown::new()));

        Box::new(
            Recogniser {
                agenda: init_heap,
                configuration_characteristic: Box::new(|c| A::extract_key(c)),
                filtered_rules: a.transition_map(),
                apply: Box::new(|c, r| r.apply(c)),
                accepting: Box::new(move |c| a.is_terminal(c)),
                item_map: Box::new(move |i| a.item_map(&i)),
                already_found: None
            }
        )
    } else {
        Box::new(empty())
    }
}

