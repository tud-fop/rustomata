use super::ChartEntry;
use std::cmp::Ordering;

#[derive(Debug, Clone, Copy)]
pub enum ChartEntryWithIndex<W> {
    Bracketed(u32, W, u32, u32, W, usize),
    Concat(u32, u32, u32, usize, usize),
    Initial(u8, W)
}

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

impl<W> PartialEq for ChartEntryWithIndex<W> {
    fn eq(&self, other: &Self) -> bool {
        use self::ChartEntryWithIndex::*;
        
        match (self, other) {
            (&Initial(ref t1, _), &Initial(ref t2, _)) => t1.eq(t2),
            (&Concat(ref p1, ref q1, ref q1_, ref i11, ref i12), &Concat(ref p2, ref q2, ref q2_, ref i21, ref i22))
                => (p1, q1, q1_, i11, i12).eq(&(p2, q2, q2_, i21, i22)),
            (&Bracketed(ref t1, _, ref p1, ref q1, _, ref i1), &Bracketed(ref t2, _, ref p2, ref q2, _, ref i2))
                => (t1, p1, q1, i1).eq(&(t2, p2, q2, i2)),
            _ => false
        }
    }
}

impl<W> Eq for ChartEntryWithIndex<W> {}

/// ordering with reversed index values, b/c of greatest-first heap
impl<W> PartialOrd for ChartEntryWithIndex<W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use self::ChartEntryWithIndex::*;
        
        match (self, other) {
            (&Initial(ref b, _), &Initial(ref b_, _)) => b.partial_cmp(b_),
            (&Initial(_, _), _) => Some(Ordering::Greater),
            (_, &Initial(_, _)) => Some(Ordering::Less),

            (&Bracketed(ref b1, _, _, _, _, ref i1), &Bracketed(ref b2, _, _, _, _, ref i2))
                => (i2, b1).partial_cmp(&(i1, b2)),
            (&Bracketed(_, _, _, _, _, _), _) => Some(Ordering::Greater),

            (&Concat(_, ref m1, _, ref i11, ref i12), &Concat(_, ref m2, _, ref i21, ref i22))
                => (i21, i22, m1).partial_cmp(&(i11, i12, m2)), // maybe triangulation?
            
            _ => Some(Ordering::Less)
        }
    }
}

impl<W> Ord for ChartEntryWithIndex<W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<W> ChartEntryWithIndex<W> where W: Copy {
    pub fn successors<'a>(&'a self) -> SuccessorIt<'a, W> {
        SuccessorIt(self, Index::Fst)
    }
}

impl<W> From<ChartEntry<W>> for ChartEntryWithIndex<W> where W: Copy {
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