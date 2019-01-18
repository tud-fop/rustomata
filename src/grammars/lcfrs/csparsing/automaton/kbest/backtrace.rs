use super::{RuleIdT, StateT, RangeT};
use std::{ cmp::Ordering, hash::{Hash, Hasher} };

/// This data structure represents a backtrace in cfg parsing and contains
/// indices for each successor used in k-best enumeration.
#[derive(Debug, Clone, Copy)]
pub enum IndexedBacktrace<W> {
    Binary(RuleIdT, StateT, RangeT, StateT, W, u32, u32),
    Unary(RuleIdT, StateT, W, u32),
    Nullary(RuleIdT, W)
}
// pub struct IndexedBacktrace<W>(RuleIdT, StateT, RangeT, StateT, W, u32, u32);

impl<W> PartialEq for IndexedBacktrace<W> {
    fn eq(&self, other: &Self) -> bool {
        use self::IndexedBacktrace::*;
        match (self, other) {
            (&Nullary(r1, _), &Nullary(r2, _)) => r1 == r2,
            (&Unary(r1, _, _, i1), &Unary(r2, _, _, i2)) => (r1, i1) == (r2, i2),
            (&Binary(r1, _, m1, _, _, i11, i12), &Binary(r2, _, m2, _, _, i21, i22))
                => (r1, m1, i11, i12) == (r2, m2, i21, i22),
            _ => false
        }
    }
}
impl<W> Eq for IndexedBacktrace<W> {}
impl<W> Hash for IndexedBacktrace<W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::IndexedBacktrace::*;
        match *self {
            Nullary(r, _) => r.hash(state),
            Unary(r, _, _, i) => (r, i).hash(state),
            Binary(r, _, _, _, _, i1, i2) => (r, i1, i2).hash(state)
        } 
    }
}
impl<W> PartialOrd for IndexedBacktrace<W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<W> Ord for IndexedBacktrace<W> {
    fn cmp(&self, other: &Self) -> Ordering {
        use self::IndexedBacktrace::*;
        match (self, other) {
            (&Nullary(r1, _), &Nullary(ref r2, _)) => r1.cmp(r2),
            (&Nullary(_, _), _) => Ordering::Greater,
            (_, Nullary(_, _)) => Ordering::Less,
            (&Unary(r1, _, _, i1), &Unary(r2, _, _, i2)) => (i2, r1).cmp(&(i1, r2)),
            (&Unary(_, _, _, _), _) => Ordering::Greater,
            (&Binary(r1, _, m1, _, _, i11, i12), &Binary(r2, _, m2, _, _, i21, i22))
                => (i21, i22, r1, m1).cmp(&(i11, i12, r2, m2)),
            (_, _) => Ordering::Less
        }
    }
}
impl<W: Copy> IndexedBacktrace<W> {
    pub fn iter(&self) -> Iter<W> { self.into_iter() }
}

pub struct Iter<'a, W>(&'a IndexedBacktrace<W>, u8);

impl<'a, W: Copy> Iterator for Iter<'a, W> {
    type Item = IndexedBacktrace<W>;
    fn next(&mut self) -> Option<Self::Item> {
        use self::IndexedBacktrace::*;
        match *self {
            Iter(&Binary(r,q1,m,q2,w,i1,i2), 2) => { self.1 = 1; Some(Binary(r,q1,m,q2,w,i1,i2+1)) },
            Iter(&Binary(r,q1,m,q2,w,i1,i2), 1) => { self.1 = 0; Some(Binary(r,q1,m,q2,w,i1+1,i2)) },
            Iter(&Unary(r,q,w,i), 1) => { self.1 = 0; Some(Unary(r,q,w,i+1)) },
            _ => None
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) { (self.1 as usize, Some(self.1 as usize)) }
}
impl<'a, W: Copy> ExactSizeIterator for Iter<'a, W> {
    fn len(&self) -> usize { self.1 as usize }
}

impl<'a, W: Copy> IntoIterator for &'a IndexedBacktrace<W> {
    type IntoIter = Iter<'a, W>;
    type Item = IndexedBacktrace<W>;
    fn into_iter(self) -> Self::IntoIter {
        use self::IndexedBacktrace::*;
        match *self {
            Binary(_, _, _, _, _, _, _) => Iter(self, 2),
            Unary(_, _, _, _) => Iter(self, 1),
            _ => Iter(self, 0)
        }
    }
}