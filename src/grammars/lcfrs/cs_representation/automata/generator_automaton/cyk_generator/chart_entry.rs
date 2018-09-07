use std::cmp::Ordering;
use std::hash::{ Hash, Hasher };

/// An entry in the `GenerationChart` for the extraction of dyck words from an fsa.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
pub enum ChartEntry<W> {
    Initial(usize, W),
    Concat(usize, usize, usize),
    Bracketed(usize, W, usize, usize, W)
}

/// A `ChartEntry` providing an index for each of its backrefs.
/// Needed for our implementation of Better k-best Parsing by Huang and Chiang.
#[derive(Debug, Clone, Copy)]
pub enum ChartEntryWithIndex<W> {
    Bracketed(usize, W, usize, usize, W, usize),
    Concat(usize, usize, usize, usize, usize),
    Initial(usize, W)
}

/// Each `ChartEntryWithIndex` has at most two backrefs.
#[derive(Debug, Clone, Copy)]
enum Index {
    Fst, Snd, Ooi
}

impl Index {
    pub fn succ(self) -> Self {
        use self::Index::*;

        match self {
            Fst => Snd,
            _ => Ooi
        }
    }
}

/// An Iterator over all `ChartEntryWithIndex` where exactly one index is incremented.
pub struct SuccessorIt<'a, W: 'a>(&'a ChartEntryWithIndex<W>, Index);

impl<'a, W> Iterator for SuccessorIt<'a, W> where W: Copy {
    type Item = ChartEntryWithIndex<W>;
    
    fn next(&mut self) -> Option<Self::Item> {
        use self::ChartEntryWithIndex::*;
        use self::Index::*;

        let result = match self {
            // &SucessorIt(&Initial(_, _), _) => None,
            &mut SuccessorIt(&Bracketed(b, w1, p, q, w2, i), Fst) => Some(Bracketed(b, w1, p, q, w2, i+1)),
            &mut SuccessorIt(&Concat(p, q1, q2, i1, i2), Fst) => Some(Concat(p, q1, q2, i1 + 1, i2)),
            &mut SuccessorIt(&Concat(p, q1, q2, i1, i2), Snd) => Some(Concat(p, q1, q2, i1, i2 + 1)),
            _ => None
        };
        self.1 = self.1.succ();
        result
    }
}

/// Drop comparison of weight.
impl<W> PartialEq for ChartEntryWithIndex<W> {
    fn eq(&self, other: &Self) -> bool {
        use self::ChartEntryWithIndex::*;
        
        match (self, other) {
            (&Initial(ref t1, _), &Initial(ref t2, _)) => t1.eq(t2),
            (&Concat(_, ref q1, _, ref i11, ref i12), &Concat(_, ref q2, _, ref i21, ref i22))
                => (q1, i11, i12).eq(&(q2, i21, i22)),
            (&Bracketed(ref t1, _, _, _, _, ref i1), &Bracketed(ref t2, _, _, _, _, ref i2))
                => (t1, i1).eq(&(t2, i2)),
            _ => false
        }
    }
}

impl<W> Hash for ChartEntryWithIndex<W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::ChartEntryWithIndex::*;
        
        match *self {
            Initial(ref t1, _) => t1.hash(state),
            Concat(_, ref q1, _, ref i11, ref i12) => (q1, i11, i12).hash(state),
            Bracketed(ref t1, _, _, _, _, ref i1) => (t1, i1).hash(state),
        }
    }
}

impl<W> Eq for ChartEntryWithIndex<W> {}

impl<W> PartialOrd for ChartEntryWithIndex<W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// ´Ord` instance with reversed index values.
impl<W> Ord for ChartEntryWithIndex<W> {
    fn cmp(&self, other: &Self) -> Ordering {
        use self::ChartEntryWithIndex::*;
        
        match (self, other) {
            (&Initial(ref b, _), &Initial(ref b_, _)) => b.cmp(b_),
            (&Initial(_, _), _) => Ordering::Greater,
            (_, &Initial(_, _)) => Ordering::Less,

            (&Bracketed(ref b1, _, _, _, _, ref i1), &Bracketed(ref b2, _, _, _, _, ref i2))
                => (i2, b1).cmp(&(i1, b2)),
            (&Bracketed(_, _, _, _, _, _), _) => Ordering::Greater,

            (&Concat(_, ref m1, _, ref i11, ref i12), &Concat(_, ref m2, _, ref i21, ref i22))
                => (i21, i22, m1).cmp(&(i11, i12, m2)), // maybe triangulation?
            
            _ => Ordering::Less
        }
    }
}

impl<W> ChartEntryWithIndex<W>
where
    W: Copy
{
    /// An Iterator over all `ChartEntryWithIndex` where exactly one index is incremented.
    pub fn successors<'a>(&'a self) -> SuccessorIt<'a, W> {
        SuccessorIt(self, Index::Fst)
    }
}

/// Adds zero indices.
impl<W> From<ChartEntry<W>> for ChartEntryWithIndex<W>
where
    W: Copy
{
    fn from(e: ChartEntry<W>) -> Self {
        use self::ChartEntryWithIndex::*;
        
        match e {
            ChartEntry::Initial(b, w) => Initial(b, w),
            ChartEntry::Bracketed(b, w1, p, q, w2) => Bracketed(b, w1, p, q, w2, 0),
            ChartEntry::Concat(p, q1, q2) => Concat(p, q1, q2, 0, 0)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn successor() {
        assert_eq!(
            ChartEntryWithIndex::Initial(0, ()).successors().collect::<Vec<_>>(),
            Vec::new()
        );
        assert_eq!(
            ChartEntryWithIndex::Bracketed(1, "W1", 3, 4, "W2", 99).successors().collect::<Vec<_>>(),
            vec![ChartEntryWithIndex::Bracketed(1, "W1", 3, 4, "W2", 100)]
        );
        let ce: ChartEntryWithIndex<()> = ChartEntryWithIndex::Concat(77, 88, 99, 11, 33);
        assert_eq!(
            ce.successors().collect::<Vec<_>>(),
            vec![ChartEntryWithIndex::Concat(77, 88, 99, 12, 33),
                 ChartEntryWithIndex::Concat(77, 88, 99, 11, 34)
                ]
        )
    }
}