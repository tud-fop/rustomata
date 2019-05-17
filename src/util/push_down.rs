use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;
use std::vec::IntoIter;

use integeriser::{HashIntegeriser, Integeriser};
use crate::util::integerisable::Integerisable1;

#[derive(Debug, Clone, PartialOrd, Ord)]
pub enum Pushdown<A> {
    Empty,
    Cons { value: A, below: Rc<Pushdown<A>> },
}

impl<A: PartialEq> PartialEq for Pushdown<A> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Pushdown::Empty, &Pushdown::Empty) => true,
            (&Pushdown::Cons {
                 value: ref a1,
                 below: ref b1,
             },
             &Pushdown::Cons {
                 value: ref a2,
                 below: ref b2,
             }) => a1 == a2 && (Rc::ptr_eq(b1, b2) || b1 == b2),
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
        Pushdown::Cons {
            value: a,
            below: Rc::new(self),
        }
    }

    /// Replaces the top-most symbol of the `Pushdown` and returns the result.
    /// If the `Pushdown` is empty, then it is returned unchanged.
    pub fn set(self, a: A) -> Result<Self, Self> {
        match self {
            Pushdown::Empty => Err(Pushdown::Empty),
            Pushdown::Cons { below, .. } => Ok(Pushdown::Cons { value: a, below }),
        }
    }

    /// Returns `true` iff the `Pushdown` is empty.
    pub fn is_empty(&self) -> bool {
        match *self {
            Pushdown::Empty => true,
            _ => false,
        }
    }

    /// Applies a function `FnMut(&A) -> B` to every node in a `Pushdown<A>`.
    pub fn map<F, B>(&self, f: &mut F) -> Pushdown<B>
    where
        F: FnMut(&A) -> B,
    {
        match *self {
            Pushdown::Empty => Pushdown::Empty,
            Pushdown::Cons {
                ref value,
                ref below,
            } => Pushdown::Cons {
                value: f(value),
                below: Rc::new(below.map(f)),
            },
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

    pub fn peek_ref(&self) -> Option<&A> {
        match *self {
            Pushdown::Empty => None,
            Pushdown::Cons { ref value, .. } => Some(value),
        }
    }

    /// Returns `Some` clone of the top-most symbol of the `Pushdown` if the `Pushdown` is not empty.
    pub fn peek(&self) -> Option<A> {
        match *self {
            Pushdown::Empty => None,
            Pushdown::Cons { ref value, .. } => Some(value.clone()),
        }
    }

    pub fn to_vec(&self) -> Vec<A> {
        let mut current = self;
        let mut res = Vec::new();
        loop {
            match *current {
                Pushdown::Empty => break,
                Pushdown::Cons {
                    ref value,
                    ref below,
                } => {
                    res.push(value.clone());
                    current = below;
                }
            }
        }
        res.reverse();
        res
    }

    pub fn iter(&self) -> IntoIter<A> {
        self.to_vec().into_iter()
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
                }
            }
        }
        res.reverse();
        res
    }
}

impl<A: Clone + Eq + Hash> Integerisable1 for Pushdown<A> {
    type AInt = Pushdown<usize>;
    type I = HashIntegeriser<A>;

    fn integerise(&self, integeriser: &mut Self::I) -> Self::AInt {
        self.map(&mut |v| integeriser.integerise(v.clone()))
    }

    fn un_integerise(aint: &Self::AInt, integeriser: &Self::I) -> Self {
        aint.map(&mut |&v| integeriser.find_value(v).unwrap().clone())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_pushdown_legal_operations_correctness() {
        assert_eq!(
            Pushdown::from(vec![1, 2].as_slice()),
            Pushdown::new()
                .push(1)
                .push(1)
                .set(2)
                .unwrap()
                .push(3)
                .pop()
                .unwrap()
                .0
        );
    }

    #[test]
    fn test_pushdown_illegal_operations_on_empty() {
        if let Ok(_) = Pushdown::<u8>::new().pop() {
            panic!("Popping an empty pushdown didn't yield an error!");
        }

        if let Some(_) = Pushdown::<u8>::new().peek() {
            panic!("Peeking an empty pushdown didn't yield an error!");
        }
    }

    #[test]
    fn test_pushdown_eq() {
        let pushdown1 = Pushdown::new().push(1).push(2);
        let pushdown2 = Pushdown::new().push(1).push(2);
        assert_eq!(pushdown1, pushdown2);
        assert_eq!(pushdown1, pushdown1.clone());

        let pushdown1 = pushdown1.push(3);
        assert_ne!(pushdown1, pushdown2);

        let pushdown2 = pushdown2.push(3);
        assert_eq!(pushdown1, pushdown2);
    }

    #[test]
    fn test_pushdown_push_inverse() {
        assert_eq!(Pushdown::new(), Pushdown::new().push(1).pop().unwrap().0)
    }

    #[test]
    fn test_pushdown_map_correctness() {
        let pushdown = Pushdown::new().push(1).push(2).push(3);

        assert_eq!(
            Pushdown::new().push(2).push(4).push(6),
            pushdown.map(&mut |x| x * 2)
        );
    }

    #[test]
    fn test_pushdown_map_inverse() {
        let pushdown = Pushdown::new().push(1).push(2).push(3);
        let mapped_pushdown = pushdown.map(&mut |x| x * 2);

        assert_eq!(pushdown, mapped_pushdown.map(&mut |x| x / 2));
    }

    #[test]
    fn test_pushdown_to_vec_correctness() {
        let pushdown = Pushdown::new().push(1).push(2).push(3);

        assert_eq!(vec![1, 2, 3], pushdown.to_vec());
    }

    #[test]
    fn test_pushdown_to_vec_inverse() {
        let pushdown = Pushdown::new().push(1).push(2).push(3);

        assert_eq!(pushdown, Pushdown::from(pushdown.to_vec().as_slice()));
    }

    #[test]
    fn test_pushdown_integerise_inverse() {
        let pushdown = Pushdown::new().push(1).push(2).push(3);
        let mut integeriser = HashIntegeriser::new();
        let integerised_pushdown = pushdown.integerise(&mut integeriser);

        assert_eq!(
            pushdown,
            Pushdown::un_integerise(&integerised_pushdown, &integeriser)
        );
    }
}
