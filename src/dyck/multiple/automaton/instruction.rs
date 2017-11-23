use std::collections::BTreeSet;
use Instruction;
use TreeStack;


/// An element of the tree push-down for recognizing a MDL.
#[derive(Debug, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub enum MDTreeElem<T> {
    Root,
    Node(Option<T>, BTreeSet<T>),
}


/// Instruction of an `Automaton` that recognizes multiple Dyck languages
/// over an alphabet of elements in `T`.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum MultipleDyckInstruction<T: Ord> {
    Up(T, BTreeSet<T>),
    // UpAt(usize, T, BTreeSet<T>),
    Down(T),
}


impl<T: Clone + Ord> Instruction<TreeStack<MDTreeElem<T>>> for MultipleDyckInstruction<T> {
    fn apply(&self, ts: TreeStack<MDTreeElem<T>>) -> Vec<TreeStack<MDTreeElem<T>>> {
        use self::MultipleDyckInstruction::{Down, Up};

        match self {
            &Up(ref symbol, ref cell) => {
                let mut succ = Vec::new();

                // option 1: push a new child
                let mut subset = cell.clone();
                subset.remove(symbol);
                succ.push(ts.clone().push_next(
                    MDTreeElem::Node(Some(symbol.clone()), subset),
                ));

                // option 2: up nondeterministcally
                for ts_ in ts.ups() {
                    if let &MDTreeElem::Node(None, ref subset) = ts_.current_symbol() {
                        if subset.contains(&symbol) {
                            let mut succset = subset.clone();
                            succset.remove(&symbol);
                            succ.push(ts_.clone().set(
                                MDTreeElem::Node(Some(symbol.clone()), succset),
                            ));
                        }
                    }
                }

                succ
            }
            &Down(ref symbol) => {
                match ts.current_symbol() {
                    &MDTreeElem::Node(Some(ref symbol_), ref subset) => {
                        if symbol != symbol_ {
                            Vec::new()
                        } else if subset.is_empty() {
                            // pop
                            match ts.clone().pop() {
                                Ok(t) => vec![t],
                                _ => Vec::new(),
                            }
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
            //_ => Vec::new(),
        }
    }
}
