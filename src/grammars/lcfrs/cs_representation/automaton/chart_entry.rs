use super::twin_state::TwinState;

use std::{ hash::{Hash, Hasher}, cmp::Ordering };

/// A `ChartEnrty` is a backtrace in a `Chart`.
#[derive(Debug, Clone, Copy)]
pub enum ChartEntry<W> {
    Initial{ label: usize, weight: W },
    Concat{ mid_state: usize, mid_range: usize },
    Wrap{ label: usize, inner: TwinState, weight: W }
}

impl<W> PartialEq for ChartEntry<W> {
    fn eq(&self, other: &Self) -> bool {
        use self::ChartEntry::*;
        
        match (self, other) {
            (&Initial{ label: ref l1, .. }, &Initial{ label: ref l2, .. }) => l1.eq(l2),
            (&Concat{ mid_range: r1, mid_state: s1 },
             &Concat{ mid_range: r2, mid_state: s2 })
                => (r1, s1).eq(&(r2, s2)),
            (&Wrap{ label: ref l1, .. }, &Wrap{ label: ref l2, .. })
                => l1.eq(l2),
            _ => false
        }
    }
}

impl<W> Eq for ChartEntry<W> {}

impl<W> Hash for ChartEntry<W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::ChartEntry::*;

        match self {
            &Initial{ label, .. }
                => label.hash(state),
            &Concat{ mid_range, mid_state }
                => { mid_range.hash(state); mid_state.hash(state); }
            &Wrap{ label, .. }
                => label.hash(state)
        }
    }
} 

// A `ChartEntry` with an index for each successor.
#[derive(Debug, Clone, Copy)]
pub enum IndexedChartEntry<W> {
    Initial{ label: usize, weight: W },
    Concat{ mid_state: usize, mid_range: usize, index_left: usize, index_right: usize },
    Wrap{ label: usize, inner: TwinState, weight: W, index: usize }
}

/// Drop comparison of weight.
impl<W> PartialEq for IndexedChartEntry<W> {
    fn eq(&self, other: &Self) -> bool {
        use self::IndexedChartEntry::*;
        
        match (self, other) {
            (&Initial{ label: ref l1, .. }, &Initial{ label: ref l2, .. }) => l1.eq(l2),
            (&Concat{ mid_range: r1, mid_state: s1, index_left: li1, index_right: ri1 },
             &Concat{ mid_range: r2, mid_state: s2, index_left: li2, index_right: ri2 })
                => (r1, s1, li1, ri1).eq(&(r2, s2, li2, ri2)),
            (&Wrap{ label: l1, index: i1, .. }, &Wrap{ label: l2, index: i2, .. })
                => (l1, i1).eq(&(l2, i2)),
            _ => false
        }
    }
}

impl<W> Eq for IndexedChartEntry<W> {}

impl<W> PartialOrd for IndexedChartEntry<W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// ´Ord` instance with reversed index values.
impl<W> Ord for IndexedChartEntry<W> {
    fn cmp(&self, other: &Self) -> Ordering {
        use self::IndexedChartEntry::*;
        
        match (self, other) {
            (&Initial{ label: ref l1, .. }, &Initial{ label: ref l2, .. }) => l1.cmp(l2),
            (&Initial{ .. }, _) => Ordering::Greater,
            (_, &Initial{ .. }) => Ordering::Less,

            (&Wrap{ label: l1, index: i1, .. }, &Wrap{ label: l2, index: i2, .. })
                => (i2, l1).cmp(&(i1, l2)),
            (&Wrap{ .. }, _) => Ordering::Greater,

            (&Concat{ mid_range: r1, mid_state: s1, index_left: li1, index_right: ri1 },
             &Concat{ mid_range: r2, mid_state: s2, index_left: li2, index_right: ri2 })
                => (li2, ri2, r1, s1).cmp(&(li1, ri1, r2, s2)),
            
            _ => Ordering::Less
        }
    }
}


impl<W> Into<IndexedChartEntry<W>> for ChartEntry<W> {
    fn into(self) -> IndexedChartEntry<W> {
        match self {
            ChartEntry::Initial{ label, weight } => 
                IndexedChartEntry::Initial{ label, weight },
            ChartEntry::Concat{ mid_range, mid_state } => 
                IndexedChartEntry::Concat{ mid_range, mid_state, index_left: 0, index_right: 0 },
            ChartEntry::Wrap{ label, inner, weight } => 
                IndexedChartEntry::Wrap{ label, inner, weight, index: 0 }
        }
    }
}

/// Each `ChartEntryWithIndex` has at most two backrefs.
#[derive(Debug, Clone, Copy)]
enum Index {
    Fst, Snd, Ooi
}

impl Index {
    /// Increments the given `Index`.
    fn succ(self) -> Self {
        use self::Index::*;

        match self {
            Fst => Snd,
            _ => Ooi
        }
    }
}

impl<W> IndexedChartEntry<W> {
    /// A successor of an `IndexedChartEntry` is an `IndexedChartEntry` which
    /// equals to it except one index is incremented.
    /// This returns an `Iterator` over all successors of an
    /// `IndexedChartEntry`.
    pub fn successors<'a>(&'a self) -> SuccessorIt<'a, W> {
        SuccessorIt(self, Index::Fst)
    }
}

/// An Iterator over all `ChartEntryWithIndex` where exactly one index is incremented.
pub struct SuccessorIt<'a, W: 'a>(&'a IndexedChartEntry<W>, Index);

impl<'a, W> Iterator for SuccessorIt<'a, W> where W: Copy {
    type Item = IndexedChartEntry<W>;
    
    fn next(&mut self) -> Option<Self::Item> {
        use self::IndexedChartEntry::*;
        use self::Index::*;

        let result = match self {
            // &SucessorIt(&Initial(_, _), _) => None,
            &mut SuccessorIt(&Wrap{ label, inner, weight, index }, Fst) => 
                Some(Wrap{ label, inner, weight, index: index + 1 }),
            &mut SuccessorIt(&Concat{ mid_state, mid_range, index_left, index_right }, Fst) => 
                Some(Concat{ mid_state, mid_range, index_left: index_left + 1, index_right }),
            &mut SuccessorIt(&Concat{ mid_state, mid_range, index_left, index_right }, Snd) => 
                Some(Concat{ mid_state, mid_range, index_left, index_right: index_right + 1 }),
            _ => None
        };
        
        self.1 = self.1.succ();
        result
    }
}