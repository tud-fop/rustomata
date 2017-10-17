use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct Node<A> {
    parent: Option<(u8, Rc<Node<A>>)>,
    value: A,
    children: Vec<Option<Rc<Node<A>>>>,
}

impl<A: PartialEq> Node<A> {
    pub fn new(a: A) -> Node<A>{
        Node { value: a, children: Vec::new(), parent: None }
    }

    pub fn map<B>(&self, f: &Fn(&A) -> B) -> Node<B> {
        let new_parent = match self.parent {
            Some((i, ref p)) => Some((i, Rc::new(p.map(f)))),
            None => None,
        };
        let new_children = self.children.iter().map(|o| o.clone().map(|v| Rc::new(v.map(f)))).collect();
        Node { parent: new_parent, value: f(&self.value), children: new_children }
    }

    pub fn map_mut<B: Clone>(&self, f: &mut FnMut(&A) -> B) -> Node<B> {
        let new_parent = match self.parent {
            Some((i, ref p)) => Some((i, Rc::new(p.map_mut(f)))),
            None => None,
        };
        let new_children = self.children.iter().map(|o| o.clone().map(|v| Rc::new(v.map_mut(f)))).collect();
        Node { parent: new_parent, value: f(&self.value), children: new_children }
    }
}

impl<A: PartialEq> PartialEq for Node<A> {
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

impl<A: PartialEq> Eq for Node<A> {}


/// Upside-down tree with a designated position (the *stack pointer*) and *nodes* of type `A`.
#[derive(Debug)]
pub struct TreeStack<A> {
    current_node: Rc<Node<A>>,  // TODO replace by unique pointer when std::ptr::Unique has landed
}

impl<A: Clone> Clone for TreeStack<A> {
    fn clone(&self) -> Self {
        Self { current_node: Rc::new(self.current_node.deref().clone()) }
    }
}

impl<A: Clone + PartialEq> TreeStack<A> {
    /// Creates a new `TreeStack<A>` with root label `a`.
    pub fn new(a: A) -> TreeStack<A> {
        Self::from_node(Node::new(a))
    }

    fn from_node(n: Node<A>) -> Self {
        TreeStack { current_node: Rc::new(n) }
    }

    /// Returns `True` if the stack pointer points to the bottom node.
    pub fn is_at_bottom(&self) -> bool {
        self.current_node.parent.is_none()
    }

    /// Returns a reference to label of the current node.
    pub fn current_symbol(&self) -> &A {
        &self.current_node.value
    }

    /// Applies a function `Fn(&A) -> B`to every node in a `TreeStack<A>`.
    pub fn map<B: Clone + PartialEq>(&self, f: &Fn(&A) -> B) -> TreeStack<B> {
        TreeStack::from_node(self.current_node.map(f))
    }

    /// Applies a function `FnMut(&A) -> B`to every node in a `TreeStack<A>`.
    pub fn map_mut<B: Clone + PartialEq>(&self, f: &mut FnMut(&A) -> B) -> TreeStack<B> {
        TreeStack::from_node(self.current_node.map_mut(f))
    }

    /// Replaces the current value by the given value.
    pub fn set(mut self, a: A) -> Self {
        {
            let mut cn = match Rc::get_mut(&mut self.current_node) {
                Some(c) => c,
                None => panic!("multiple pointers to the same `current_node`."),
            };
            cn.value = a;
        }

        self
    }

    /// Writes a value to the specified child position (if the child position is vacant) and returns the resulting `TreeStack` in an `Ok`.
    /// Returns the unmodified `TreeStack` in an `Err` otherwise.
    pub fn push(mut self, n: u8, a: A) -> Result<Self, Self> {
        if {
            let mut cn = match Rc::get_mut(&mut self.current_node) {
                Some(c) => c,
                None => panic!("multiple pointers to the same `current_node`."),
            };

            let filler = &mut vec![None; usize::from(n) - cn.children.len() + 1];
            cn.children.append(filler);

            cn.children[usize::from(n)].is_none()
        } {
            Ok(Self::from_node(Node { value: a,
                                      children: Vec::new(),
                                      parent: Some((n, self.current_node)) }))
        } else {
            Err(self)
        }
    }

    /// Goes up to a specific child position (if this position is occupied) and returns the resulting `TreeStack` in an `Ok`.
    /// Returns the unmodified `TreeStack` in an `Err` otherwise.
    pub fn up(mut self, n: u8) -> Result<Self, Self> {
        match {
            let mut cn = match Rc::get_mut(&mut self.current_node) {
                Some(c) => c,
                None => panic!("multiple pointers to the same `current_node`."),
            };

            if cn.children.len() > usize::from(n) {
                cn.children.push(None);
                cn.children.swap_remove(usize::from(n))
            } else {
                None
            }
        } {
            Some(tn) => Ok(Self::from_node(Node { value: tn.value.clone(),
                                                  children: tn.children.clone(),
                                                  parent: Some((n, self.current_node)) })),
            None => Err(self),
        }
    }

    /// Goes down to the parent position (if there is one) and returns the resulting `TreeStack` in an `Ok`.
    /// Returns the unmodified `TreeStack` in an `Err` otherwise.
    pub fn down(mut self) -> Result<Self, Self> {
        match {
            let mut cn = match Rc::get_mut(&mut self.current_node) {
                Some(c) => c,
                None => panic!("multiple pointers to the same `current_node`."),
            };

            cn.parent.take()
        } {
            Some((n, pn)) => {
                let mut new_pch = pn.children.clone();
                new_pch[usize::from(n)] = Some(self.current_node);
                Ok(Self::from_node(Node { value: pn.value.clone(),
                                          children: new_pch,
                                          parent: pn.parent.clone() }))
            },
            None => Err(self),
        }
    }
}

#[test]
fn test_tree_stack() {
    let mut ts: TreeStack<u8> = TreeStack::new(0);
    assert_eq!(&0, ts.current_symbol());

    ts = ts.push(1, 1).unwrap().clone();
    assert_eq!(&1, ts.current_symbol());

    ts = ts.down().unwrap().clone();
    assert_eq!(&0, ts.current_symbol());

    ts = ts.push(2, 2).unwrap().clone();
    assert_eq!(&2, ts.current_symbol());

    ts = ts.down().unwrap().clone();
    ts = ts.up(1).unwrap().clone();
    assert_eq!(&1, ts.current_symbol());

    let ts1 = ts.clone().push(1, 11);
    let ts2 = ts.push(1, 11);
    assert_eq!(ts1, ts2);
}


impl<A: PartialEq> PartialEq for TreeStack<A> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.current_node, &other.current_node)
            || self.current_node == other.current_node
    }
}

impl<A: PartialEq> Eq for TreeStack<A> {}
