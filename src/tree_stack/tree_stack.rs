use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt;


/// Upside-down tree with a designated position (the *stack pointer*) and *nodes* of type `A`.
/// We always assume that `self.contains_key(pointer) == true`.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TreeStack<A: Ord> {
    pub tree: HashMap<Vec<u8>, A>,
    pub pointer: Vec<u8>,
}

impl<A: Ord + PartialEq + Clone + Debug> TreeStack<A> {
    /// Creates a new `TreeStack<A>` with root label `a`.
    pub fn new(a: A) -> TreeStack<A> {
        let mut tree: HashMap<Vec<u8>, A> = HashMap::new();
        tree.insert(Vec::new(), a);
        TreeStack {
            tree: tree,
            pointer: Vec::new(),
        }
    }

    /// Returns `True` if the stack pointer points to the bottom node.
    pub fn is_at_bottom(&self) -> bool {
        self.pointer.is_empty()
    }

    /// Returns a reference to the node currently under the stack pointer.
    pub fn current_symbol(&self) -> &A {
        self.tree.get(&self.pointer).unwrap()
    }

    /// Returns `None` if the current node is different from `current_val`.
    /// If `old_val == None`, then this method checks if the `n`th child is vacant and pushes `new_val` there.
    /// If `old_val == Some(x)`, then this method checks if the `n`th child is `old_val` and goes there, replacing `old_val` with `new_val`.
    pub fn up(&self,
              n: u8,
              current_val: &A,
              old_val: Option<&A>,
              new_val: &A)
              -> Vec<TreeStack<A>> {
        let mut new_pointer = self.pointer.clone();
        new_pointer.push(n);

        match (self.tree.get(&self.pointer), self.tree.get(&new_pointer).clone()) {
            (Some(val), o_val) if val == current_val && o_val == old_val => {
                let mut new_tree = self.tree.clone();
                new_tree.insert(new_pointer.clone(), new_val.clone());
                vec![TreeStack { tree: new_tree, pointer: new_pointer }]
            }
            _ => Vec::new(),
        }
    }

    /// Moves the `pointer` to the parent node if one exists and returns `None` otherwise.
    pub fn down(&self, current_val: &A, old_val: &A, new_val: &A) -> Vec<TreeStack<A>> {
        if self.pointer.is_empty() {
            Vec::new()
        } else {
            let mut new_pointer = self.pointer.clone();
            new_pointer.pop();
            match (self.tree.get(&self.pointer), self.tree.get(&new_pointer)) {
                (Some(c_val), Some(o_val)) if c_val == current_val && o_val == old_val => {
                    let mut new_tree = self.tree.clone();
                    new_tree.insert(new_pointer.clone(), new_val.clone());
                    vec![TreeStack { tree: new_tree, pointer: new_pointer }]
                },
                _ => Vec::new(),
            }
        }
    }
}

impl<A: Ord + Clone + Debug + fmt::Display> fmt::Display for TreeStack<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        write!(f, "current symbol:{}", self.current_symbol())
    }
}
