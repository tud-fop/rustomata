pub mod instruction;

use std::collections::{BinaryHeap, HashMap};
pub use dyck::multiple::automaton::instruction::{MDTreeElem, MultipleDyckInstruction};
use util::partition::Partition;
use recognisable::Transition;
use tree_stack_automaton::TreeStack;
use recognisable::Configuration;

use dyck::Bracket;
use recognisable::automaton::{Automaton, TransitionMap};

use recognisable::Item;
use std::rc::Rc;
type MDTransition<T> = Transition<MultipleDyckInstruction<T>, Bracket<T>, u8>;

/// A variant of a tree stack `Automaton` that is used to recognize multiple Dyck languages
/// over symbols in `T`.
#[derive(Debug)]
pub struct MultipleDyckAutomaton<T: Ord> {
    transitions:
        Rc<HashMap<(), BinaryHeap<Transition<MultipleDyckInstruction<T>, Bracket<T>, u8>>>>,
}

impl<T: Clone + Ord> MultipleDyckAutomaton<T> {
    /// Like a multiple Dyck language, the automaton is instantiated using a `Partition`
    /// Π = { π₁, …, πₙ } of an implicit alphabet Σ = π₁ ∪ … ∪ πₙ.
    pub fn new(partition: Partition<T>) -> Self {
        let mut heap = BinaryHeap::new();

        for cell in partition.collapse() {
            for symbol in cell {
                heap.push(Transition {
                    word: vec![Bracket::Open(symbol.clone())],
                    weight: 0,
                    instruction: MultipleDyckInstruction::Up(symbol.clone(), cell.clone()),
                });

                heap.push(Transition {
                    word: vec![Bracket::Close(symbol.clone())],
                    weight: 0,
                    instruction: MultipleDyckInstruction::Down(symbol.clone()),
                });
            }
        }

        let mut map = HashMap::new();
        map.insert((), heap);
        MultipleDyckAutomaton {
            transitions: Rc::new(map),
        }
    }

    /// Constructs a tree-stack automaton that recognizes a sorted multiple Dyck language.
    pub fn sorted<F>(partition: Partition<T>, sort: F) -> Self
    where
        F: Fn(&T) -> usize,
    {
        let mut heap = BinaryHeap::new();

        for cell in partition.collapse() {
            for symbol in cell {
                heap.push(Transition {
                    word: vec![Bracket::Open(symbol.clone())],
                    weight: 0,
                    instruction: MultipleDyckInstruction::UpAt(
                        sort(symbol),
                        symbol.clone(),
                        cell.clone(),
                    ),
                });

                heap.push(Transition {
                    word: vec![Bracket::Close(symbol.clone())],
                    weight: 0,
                    instruction: MultipleDyckInstruction::Down(symbol.clone()),
                });
            }
        }

        let mut map = HashMap::new();
        map.insert((), heap);
        MultipleDyckAutomaton {
            transitions: Rc::new(map),
        }
    }
}

use recognisable::Instruction;

impl<T> Automaton<Bracket<T>, u8> for MultipleDyckAutomaton<T>
where
    T: Clone + Ord,
{
    type I = MultipleDyckInstruction<T>;
    type Key = ();

    type IInt = MultipleDyckInstruction<T>;
    type TInt = Bracket<T>;

    fn from_transitions<It>(_: It, _: <Self::I as Instruction>::Storage) -> Self
    where
        It: IntoIterator<Item = MDTransition<T>>,
    {
        panic!("not implemented")
    }

    fn transitions<'a>(&'a self) -> Box<Iterator<Item = MDTransition<T>> + 'a> {
        Box::new(self.transitions.get(&()).unwrap().iter().cloned())
    }

    fn initial(&self) -> <Self::I as Instruction>::Storage {
        TreeStack::new(MDTreeElem::Root)
    }

    fn item_map(
        &self,
        i: &Item<<Self::IInt as Instruction>::Storage, Self::IInt, Self::TInt, u8>,
    ) -> Item<<Self::I as Instruction>::Storage, Self::I, Bracket<T>, u8> {
        i.clone()
    }

    fn terminal_to_int(&self, t: &Bracket<T>) -> Option<Self::TInt> {
        Some(t.clone())
    }

    fn extract_key(_: &Configuration<TreeStack<MDTreeElem<T>>, Bracket<T>, u8>) -> &Self::Key {
        &()
    }

    fn is_terminal(&self, c: &Configuration<TreeStack<MDTreeElem<T>>, Bracket<T>, u8>) -> bool {
        c.word.is_empty() && c.storage.is_at_bottom()
            && c.storage
                .all(&|node: &MDTreeElem<T>| -> bool { node.is_empty() })
    }

    fn transition_map(&self) -> Rc<TransitionMap<Self::Key, Self::IInt, Self::TInt, u8>> {
        Rc::clone(&self.transitions)
    }

    fn initial_int(&self) -> <Self::IInt as Instruction>::Storage {
        self.initial()
    }
}

use serde::{Serialize, Serializer};

impl<T> Serialize for MultipleDyckAutomaton<T>
where
    T: Serialize + Ord,
{
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.transitions.get(&()).unwrap().serialize(serializer)
    }
}

use serde::{Deserialize, Deserializer};
use std::iter::once;

impl<'de, T> Deserialize<'de> for MultipleDyckAutomaton<T>
where
    T: Deserialize<'de> + Ord,
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let t = BinaryHeap::deserialize(deserializer)?;
        Ok(MultipleDyckAutomaton {
            transitions: Rc::new(once(((), t)).collect()),
        })
    }
}
