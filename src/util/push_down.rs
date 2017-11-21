use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone, PartialOrd, Ord)]
pub enum Pushdown<A> {
    Empty,
    Cons { value: A, below: Rc<Pushdown<A>> },
}

impl<A: PartialEq> PartialEq for Pushdown<A> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Pushdown::Empty, &Pushdown::Empty) => true,
            (&Pushdown::Cons { value: ref a1, below: ref b1},
             &Pushdown::Cons { value: ref a2, below: ref b2}) => {
                a1 == a2 && (Rc::ptr_eq(b1, b2) || b1 == b2)
            },
            _ => false,
        }
    }
}

impl<A: Eq> Eq for Pushdown<A> {}

impl<A> Pushdown<A> {
    /// Creates an empty `Pushdown`.
    pub fn new() -> Pushdown<A> {
        Pushdown::Empty
    }

    /// Pushes an `a: A` on top of the `Pushdown`.
    pub fn push(self, a: A) -> Self {
        Pushdown::Cons { value: a, below: Rc::new(self) }
    }

    /// Replaces the top-most symbol of the `Pushdown` and returns the result.
    /// If the `Pushdown` is empty, then it is returned unchanged.
    pub fn set(self, a: A) -> Result<Self, Self> {
        match self {
            Pushdown::Empty => Err(Pushdown::Empty),
            Pushdown::Cons { below, .. } => Ok(Pushdown::Cons { value: a, below: below }),
        }
    }

    /// Returns `true` iff the `Pushdown` is empty.
    pub fn is_empty(&self) -> bool {
        match *self {
            Pushdown::Empty => true,
            _ => false,
        }
    }
}

impl<A: Clone> Pushdown<A> {
    /// Removes the top-most symbol of the `Pushdown` and returns it together with the resulting `Pushdown`.
    /// If the `Pushdown` is empty, then it is returned unchanged.
    pub fn pop(self) -> Result<(Self, A), Self> {
        match self {
            Pushdown::Empty => Err(Pushdown::Empty),
            Pushdown::Cons { value, below } => Ok((below.deref().clone(), value)),
        }
    }

    /// Returns `Some` clone of the top-most symbol of the `Pushdown` if the `Pushdown` is not empty.
    pub fn peek(&self) -> Option<A> {
        match *self {
            Pushdown::Empty => None,
            Pushdown::Cons { ref value, .. } => Some(value.clone()),
        }
    }
}

impl<A> Default for Pushdown<A> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, A: Clone> From<&'a [A]> for Pushdown<A> {
    fn from(vec: &'a [A]) -> Self {
        let mut res = Pushdown::new();
        for a in vec.iter() {
            res = res.push(a.clone());
        }
        res
    }
}

impl<A: Clone> Into<Vec<A>> for Pushdown<A> {
    fn into(self) -> Vec<A> {
        let mut current = self;
        let mut res = Vec::new();
        loop {
            match current {
                Pushdown::Empty => break,
                Pushdown::Cons { value, below } => {
                    res.push(value);
                    current = below.deref().clone();
                },
            }
        }
        res.reverse();
        res
    }
}

#[test]
fn test_pushdown() {
    assert_eq!(Pushdown::new().push(1).push(2).push(3).push(4).pop().unwrap().0,
               Pushdown::from(vec![1,2,3]));

    let v1: Vec<i32> = Pushdown::new().push(1).push(2).push(3).into();
    assert_eq!(v1, vec![1,2,3]);
}

