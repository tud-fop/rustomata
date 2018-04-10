use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::ops::MulAssign;
use std::rc::Rc;

use num_traits::One;

use recognisable::{Configuration, Instruction, Item, Transition};
use util::agenda::Capacity;
use util::push_down::Pushdown;
use util::search::Search;

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
}


pub fn recognise<'a, A, T, W>(a: &'a A, word: Vec<T>)
                              -> impl Iterator<Item=Item<<A::I as Instruction>::Storage, A::I, T, W>> + 'a
    where A: Automaton<T, W>,
          A::I: Clone + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Eq,
          A::IInt: Ord + 'a,
          <A::IInt as Instruction>::Storage: Clone + Eq + Ord,
          T: Clone + Eq + Ord,
          A::TInt: Clone + Eq + Ord + 'a,
          W: Copy + MulAssign + One + Ord + 'a,
{
    let the_words: Option<_> = word.iter().map(|t| a.terminal_to_int(t)).collect();

    let init_confs = the_words.map(
        |the_word|
        (Configuration {
            word: the_word,
            storage: a.initial_int(),
            weight: W::one(),
        }, Pushdown::new())
    );

    Search::weighted(
        init_confs,
        move |(conf, run)| {
            let key = A::extract_key(conf);
            let trans_map = a.transition_map();
            let bh = BinaryHeap::new();
            let rules = trans_map.get(key).unwrap_or(&bh);
            rules.iter().flat_map(|r| {
                r.apply(conf).into_iter().map(move |conf1| (conf1, run.clone().push(r.clone())))
            }).collect()
        }
    ).filter(move |(c, _)| a.is_terminal(c)).map(move |i| a.item_map(&i))
}


pub fn recognise_beam<'a, A, T, W>(a: &'a A, beam: usize, word: Vec<T>)
                               -> impl Iterator<Item=Item<<A::I as Instruction>::Storage, A::I, T, W>> + 'a
    where A: Automaton<T, W>,
          A::I: Clone + Eq + Instruction,
          <A::I as Instruction>::Storage: Clone + Eq,
          A::IInt: Ord + 'a,
          <A::IInt as Instruction>::Storage: Clone + Eq + Ord,
          T: Clone + Eq + Ord,
          A::TInt: Clone + Eq + Ord + 'a,
          W: Copy + MulAssign + One + Ord + 'a,
{
    let the_words: Option<_> = word.iter().map(|t| a.terminal_to_int(t)).collect();

    let init_confs = the_words.map(
        |the_word|
        (Configuration {
            word: the_word,
            storage: a.initial_int(),
            weight: W::one(),
        }, Pushdown::new())
    );

    Search::weighted(
        init_confs,
        move |(conf, run)| {
            let key = A::extract_key(conf);
            let trans_map = a.transition_map();
            let bh = BinaryHeap::new();
            let rules = trans_map.get(key).unwrap_or(&bh);
            rules.iter().flat_map(|r| {
                r.apply(conf).into_iter().map(move |conf1| (conf1, run.clone().push(r.clone())))
            }).collect()
        }
    ).beam(Capacity::Limit(beam)).filter(move |(c, _)| a.is_terminal(c)).map(move |i| a.item_map(&i))
}

