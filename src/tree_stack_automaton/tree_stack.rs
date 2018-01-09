use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;
use std::hash::Hash;
use util::integerisable::Integerisable1;
use integeriser::{HashIntegeriser, Integeriser};

/// upside-down tree with a designated position (the *stack pointer*) and *nodes* of type `a`.
#[derive(Clone, Debug)]
pub struct TreeStack<A> {
    parent: Option<(usize, Rc<TreeStack<A>>)>,
    value: A,
    children: Vec<Option<Rc<TreeStack<A>>>>,
}

impl<A> TreeStack<A> {
    /// Creates a new `TreeStack<A>` with root label `a`.
    pub fn new(a: A) -> Self {
        TreeStack { value: a, children: Vec::new(), parent: None }
    }

    /// Applies a function `Fn(&A) -> B`to every node in a `TreeStack<A>`.
    pub fn map<F, B>(&self, f: &mut F) -> TreeStack<B>
        where F: FnMut(&A) -> B,
    {
        let new_value = f(&self.value);
        let new_parent = match self.parent {
            Some((i, ref p)) => Some((i, Rc::new(p.map(f)))),
            None => None,
        };
        let new_children = self.children.iter().map(|o| o.clone().map(|v| Rc::new(v.map(f)))).collect();
        TreeStack { parent: new_parent, value: new_value, children: new_children }
    }

    /// Returns `True` if the stack pointer points to the bottom node.
    pub fn is_at_bottom(&self) -> bool {
        self.parent.is_none()
    }

    /// Returns a reference to label of the current node.
    pub fn current_symbol(&self) -> &A {
        &self.value
    }

    /// Replaces the current value by the given value.
    pub fn set(mut self, a: A) -> Self {
        self.value = a;
        self
    }

    /// Writes a value to the specified child position (if the child position is vacant) and returns the resulting `TreeStack` in an `Ok`.
    /// Returns the unmodified `TreeStack` in an `Err` otherwise.
    pub fn push(mut self, n: usize, a: A) -> Result<Self, Self> {
        if n >= self.children.len() {
            let len = n - self.children.len() + 1;
            let filler = &mut vec![None; len];
            self.children.append(filler);
        }

        if self.children[n].is_none() {
            Ok(TreeStack { value: a,
                           children: Vec::new(),
                           parent: Some((n, Rc::new(self))) })
        } else {
            Err(self)
        }
    }
}

impl<A: Clone + fmt::Display> fmt::Display for TreeStack<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (tree, pointer) = self.to_tree();

        for (path, value) in tree.iter() {
            let mut line1 = String::from(" ");
            let mut line2 = String::from(if path.eq(&pointer) {
                "*"
            } else {
                " "
            });

            match path.last() {
                Some(child_num) => {
                    for _ in 0..path.len() - 1 {
                        line1.push_str("| ");
                        line2.push_str("| ");
                    }

                    line1.push_str("|");
                    line2.push_str(&format!("+-{}: {}", child_num, value));
                },
                None => line2.push_str(&value.to_string()),
            }

            write!(f, "{}\n{}\n", line1, line2)?
        }

        Ok(())
    }
}

impl<A: Clone> TreeStack<A> {
    /// Goes up to a specific child position (if this position is occupied) and returns the resulting `TreeStack` in an `Ok`.
    /// Returns the unmodified `TreeStack` in an `Err` otherwise.
    pub fn up(mut self, n: usize) -> Result<Self, Self> {
        match {
            if self.children.len() > n {
                self.children.push(None);
                self.children.swap_remove(n)
            } else {
                None
            }
        } {
            Some(ref tn) => Ok(TreeStack { value: tn.value.clone(),
                                           children: tn.children.clone(),
                                           parent: Some((n, Rc::new(self))) }),
            _ => Err(self),
        }
    }

    /// Goes down to the parent position (if there is one) and returns the resulting `TreeStack` in an `Ok`.
    /// Returns the unmodified `TreeStack` in an `Err` otherwise.
    pub fn down(mut self) -> Result<Self, Self> {
        match self.parent.take() {
            Some((n, pn)) => {
                let mut new_pch = pn.children.clone();
                new_pch[n] = Some(Rc::new(self));
                Ok(TreeStack { value: pn.value.clone(),
                               children: new_pch,
                               parent: pn.parent.clone() })
            },
            None => Err(self),
        }
    }

    pub fn to_tree(&self) -> (BTreeMap<Vec<usize>, A>, Vec<usize>) {
        let mut tree_map = BTreeMap::new();
        let mut curr_path = Vec::new();

        if let Some((num, ref parent)) = self.parent {
            let (parent_map, parent_path) = parent.to_tree();
            curr_path = parent_path;
            curr_path.push(num);

            for (path, value) in parent_map.iter() {
                tree_map.insert(path.clone(), value.clone());
            }
        }

        tree_map.insert(curr_path.clone(), self.value.clone());

        for (num, maybe_child) in self.children.iter().enumerate() {
            if let &Some(ref child) = maybe_child {
                let (mut child_map, _) = child.to_tree();

                for (path, value) in child_map {
                    let mut new_path = curr_path.clone();
                    new_path.append(&mut path.clone());
                    new_path.push(num);
                    tree_map.insert(new_path, value.clone());
                }
            }
        }

        (tree_map, curr_path)
    }
}

impl<A: Clone + Eq + Hash> Integerisable1 for TreeStack<A> {
    type AInt = TreeStack<usize>;
    type I = HashIntegeriser<A>;

    fn integerise(&self, integeriser: &mut Self::I) -> Self::AInt {
        self.map(&mut move |v| integeriser.integerise(v.clone()))
    }

    fn un_integerise(aint: &Self::AInt, integeriser: &Self::I) -> Self {
        aint.map(&mut |&v| integeriser.find_value(v).unwrap().clone())
    }
}


impl<A: PartialEq> PartialEq for TreeStack<A> {
    fn eq(&self, other: &Self) -> bool {
        let comp = |p1, p2| Rc::ptr_eq(p1, p2) || p1 == p2;
        self.value == other.value
            && match (&self.parent, &other.parent) {
                (&Some((i1, ref p1)), &Some((i2, ref p2))) => i1 == i2 && comp(p1, p2),
                (&None, &None)                             => true,
                _                                          => false,
            }
            && self.children == other.children
    }
}

impl<A: Eq> Eq for TreeStack<A> {}

impl<A: PartialOrd> PartialOrd for TreeStack<A> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.value.partial_cmp(&other.value) {
            None | Some(Ordering::Equal) =>
                match self.parent.partial_cmp(&other.parent) {
                    None | Some(Ordering::Equal) => self.children.partial_cmp(&other.children),
                    x => x,
                }
            x => x,
        }
    }
}

impl<A: Ord> Ord for TreeStack<A> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.value.cmp(&other.value) {
            Ordering::Equal =>
                match self.parent.cmp(&other.parent) {
                    Ordering::Equal => self.children.cmp(&other.children),
                    x => x,
                }
            x => x,
        }
    }
}

#[test]
fn test_tree_stack() {
    let mut ts: TreeStack<u8> = TreeStack::new(0);
    assert_eq!(&0, ts.current_symbol());

    ts = ts.push(1, 1).unwrap();
    assert_eq!(&1, ts.current_symbol());

    ts = ts.down().unwrap();
    assert_eq!(&0, ts.current_symbol());

    ts = ts.push(2, 2).unwrap();
    assert_eq!(&2, ts.current_symbol());

    ts = ts.down().unwrap();
    ts = ts.up(1).unwrap();
    assert_eq!(&1, ts.current_symbol());

    ts = ts.push(1, 11).unwrap();
    assert_eq!(&11, ts.current_symbol());

    ts = ts.down().unwrap();
    ts = ts.down().unwrap();
    ts = ts.up(2).unwrap();
    ts = ts.push(1, 21).unwrap();
    assert_eq!(&21, ts.current_symbol());

    ts = ts.down().unwrap();
    ts = ts.down().unwrap();

    println!("Display: {}", &ts);
    println!("Debug: {:?}", &ts);
}
