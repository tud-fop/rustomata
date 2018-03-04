use std::collections::BTreeSet;
use Instruction;
use TreeStack;


/// An element of the tree push-down for recognizing a MDL.
#[derive(Debug, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum MDTreeElem<T> {
    Root,
    Node(Option<T>, BTreeSet<T>),
}

impl<T: Ord> MDTreeElem<T> {
    /// Returns true, if
    /// * the node is the root, or
    /// * the node is (None, ∅).
    pub fn is_empty(&self) -> bool {
        match *self {
            MDTreeElem::Root => true,
            MDTreeElem::Node(None, ref set) => set.is_empty(),
            _ => false
        }
    }
}


/// Instruction of an `Automaton` that recognizes multiple Dyck languages
/// over an alphabet of elements in `T`.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Serialize, Deserialize)]
pub enum MultipleDyckInstruction<T: Ord> {
    /// Nondeterministic `Up` for multiple Dyck languages.
    /// Returns a tree stack for each child with a set that contains the symbol and
    /// a tree stack with a freshly pushed child node.
    Up(T, BTreeSet<T>),
    /// Moves up if the child at the specified position
    /// * is vacant, or
    /// * contains the symbol in its set.
    UpAt(usize, T, BTreeSet<T>),
    /// Moves down.
    Down(T),
}


impl<T: Clone + Ord> Instruction for MultipleDyckInstruction<T> {
    type Storage = TreeStack<MDTreeElem<T>>;
    
    fn apply(&self, ts: TreeStack<MDTreeElem<T>>) -> Vec<TreeStack<MDTreeElem<T>>> {
        use self::MultipleDyckInstruction::*;

        match *self {
            UpAt(position, ref symbol, ref cell) => {
                match ts.push_with(position, 
                    || { // try to push node in this position
                        let mut c = cell.clone();
                        c.remove(symbol);
                        MDTreeElem::Node(Some(symbol.clone()), c)
                    }
                ) { // if node is blocked check the set in it and go up
                    Ok(ts) => vec![ts],
                    Err(ts) => {
                        let ts_ = ts.up(position).ok().unwrap();
                        if let Some(mut succset) = {
                            if let MDTreeElem::Node(None, ref subset) = *ts_.current_symbol() {
                                if subset.contains(symbol) {
                                    Some(subset.clone())
                                } else { None }
                            } else { None }
                        } {
                            succset.remove(symbol);
                            vec![ts_.set(MDTreeElem::Node(Some(symbol.clone()), succset))]
                        } else {
                            vec![]
                        }
                    }
                }
            },
            Up(ref symbol, ref cell) => {
                let mut succ = Vec::new();

                // option 1: push a new child
                let mut subset = cell.clone();
                subset.remove(symbol);
                succ.push(
                    ts.clone().push_next(
                        MDTreeElem::Node(Some(symbol.clone()), subset),
                    )
                );

                // option 2: up nondeterministcally
                for ts_ in ts.ups() {
                    if let MDTreeElem::Node(None, ref subset) = *ts_.current_symbol() {
                        if subset.contains(symbol) {
                            let mut succset = subset.clone();
                            succset.remove(symbol);
                            succ.push(
                                ts_.clone().set(
                                    MDTreeElem::Node(Some(symbol.clone()), succset),
                                )
                            );
                        }
                    }
                }

                succ
            }
            Down(ref symbol) => {
                match *ts.current_symbol() {
                    MDTreeElem::Node(Some(ref symbol_), ref subset) => {
                        if symbol != symbol_ {
                            Vec::new()
                        } else {
                            // down
                            match ts.clone()
                                .set(MDTreeElem::Node(None, subset.clone()))
                                .down() {
                                Ok(t) => vec![t],
                                _ => Vec::new(),
                            }
                        }
                    }
                    _ => Vec::new(),
                }
            }
        }
    }
}
