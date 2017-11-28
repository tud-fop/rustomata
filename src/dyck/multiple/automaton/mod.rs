mod instruction;

use std::collections::{HashMap, BinaryHeap};
use dyck::multiple::automaton::instruction::{MDTreeElem, MultipleDyckInstruction};
use util::partition::Partition;
use std::fmt::Debug;

use Transition;
use TreeStack;
use automata::Automaton;
use Configuration;

use dyck::Bracket;


pub type Trans<T> = Transition<TreeStack<MDTreeElem<T>>, MultipleDyckInstruction<T>, Bracket<T>, u8>;

/// A variant of a tree stack `Automaton` that is used to recognize multiple Dyck languages
/// over symbols in `T`.
#[derive(Debug)]
pub struct MultipleDyckAutomaton<T: Ord + Clone> {
    transitions: HashMap<
        (),
        BinaryHeap<Trans<T>>,
    >,
}

impl<T: Ord + Clone> MultipleDyckAutomaton<T> {
    /// Like a multiple Dyck language, the automaton is instantiated using a `Partition`
    /// Π = { π₁, …, πₙ } of an implicit alphabet Σ = π₁ ∪ … ∪ πₙ.
    pub fn new(partition: &Partition<T>) -> Self {
        use std::marker::PhantomData;

        let mut heap = BinaryHeap::new();
        for symbol in partition.alphabet() {
            heap.push(Transition {
                _dummy: PhantomData,
                word: vec![Bracket::Open(symbol.clone())],
                weight: 0,
                instruction: MultipleDyckInstruction::Up(
                    symbol.clone(),
                    partition.get_cell(symbol).unwrap().clone(),
                ),
            });
            heap.push(Transition {
                _dummy: PhantomData,
                word: vec![Bracket::Close(symbol.clone())],
                weight: 0,
                instruction: MultipleDyckInstruction::Down(symbol.clone()),
            });
        }

        let mut map = HashMap::new();
        map.insert((), heap);
        MultipleDyckAutomaton { transitions: map }
    }
}

impl<
    T: Clone + Eq + Debug + Ord,
> Automaton<TreeStack<MDTreeElem<T>>, MultipleDyckInstruction<T>, Bracket<T>, u8>
    for MultipleDyckAutomaton<T> {
    type Key = ();

    fn extract_key(_: &Configuration<TreeStack<MDTreeElem<T>>, Bracket<T>, u8>) -> &() {
        &()
    }

    fn transitions(
        &self,
    ) -> &HashMap<
        (),
        BinaryHeap<
            Transition<
                TreeStack<MDTreeElem<T>>,
                MultipleDyckInstruction<T>,
                Bracket<T>,
                u8,
            >,
        >,
    > {
        &self.transitions
    }

    fn initial(&self) -> TreeStack<MDTreeElem<T>> {
        TreeStack::new(MDTreeElem::Root)
    }

    fn is_terminal(&self, conf: &Configuration<TreeStack<MDTreeElem<T>>, Bracket<T>, u8>) -> bool {
        conf.word.is_empty() && conf.storage == TreeStack::new(MDTreeElem::Root)
    }
}
