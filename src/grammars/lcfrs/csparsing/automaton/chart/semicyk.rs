use util::IntMap;
use super::{Chart, IndexedChartEntry, ChartEntry, TwinRange, super::TwinState};
use unique_heap::FnvUniqueHeap;
use std::mem::swap;
use std::slice::Iter;
use fnv::FnvHashMap;

type StateT = usize;
type RangeT = usize;
type TerminalT = usize;



// enum CykBacktrace {
//     WrapsAndConcats(StateT, RangeT, TerminalT, TerminalT),
//     WrapRightAndConcat(StateT, RangeT, TerminalT),
//     ConcatAndWrap(TerminalT, StateT, RangeT),
//     DoubleWrap(TerminalT, TerminalT, StateT, StateT),
//     InitialAndWrap(TerminalT, TerminalT)
// }

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EntryState {
    Left, Right, Unset
}

impl EntryState {
    pub unsafe fn lock(left: &Self, right: &Self) -> bool {
        match (left, right) {
            (&EntryState::Right, _) | (_, &EntryState::Left) => false,
            _ => {
                let lp = (left as *const EntryState) as *mut EntryState; *lp = EntryState::Left;
                let rp = (right as *const EntryState) as *mut EntryState; *rp = EntryState::Right;
                true
            }
        }
    }
}

// map from state ranges to viterbi values
// type VitMapL<W> = FnvHashMap<(StateT, StateT), W>;
// // 2-stepped map from state ranges to viterbi values
// // to support access by left component
// type VitMapU<W> = FnvHashMap<StateT, FnvHashMap<StateT, W>>;
// type VitMapR<W> = FnvHashMap<StateT, Vec<(StateT, W)>>;

pub type VitMap<W> = FnvHashMap<StateT, FnvHashMap<StateT, (W, EntryState)>>;
// maps state ranges to backlinks
pub type BaLiMap<W> = FnvHashMap<(StateT, StateT), Vec<(ChartEntry<W>, W)>>;
pub type ChartCell<W> = (VitMap<W>, BaLiMap<W>);


#[derive(Clone, Debug)]
pub struct SemiCykChart<W>(
    Vec<ChartCell<W>>,
    usize,  // word length
    StateT, // initial state
    StateT  // final state
);
 
use std::fmt::Display;
use integeriser::{Integeriser, HashIntegeriser};
use std::rc::Rc;
use std::hash::Hash;
impl<W> SemiCykChart<W> {
    pub fn to_pretty_string<T>(&self, int: &Rc<HashIntegeriser<T>>) -> String
    where
        W: Display,
        T: Display + Eq + Hash + Clone
    {
        let mut buf = String::new();
        for range in 1..=(self.1) {
            for l in 0..=(self.1 - range) {
                let r = l + range;
                let index = chart_index(l, r, self.1);
                buf.push_str(&format!("{}, {}:\n", l, r));
                
                let mut viterbis: FnvHashMap<(StateT, StateT), &W> = FnvHashMap::default();
                for (left, right, ref w) in self.0[index].0.iter().flat_map(|(left, rwe)| rwe.iter().map(move |(right, &(ref w, _))| (*left, *right, w))) {
                    viterbis.insert((left, right), w);
                }
                
                for (&(statel, stater), backtraces) in &self.0[index].1 {
                    let viterbi = viterbis.get(&(statel, stater)).unwrap();
                    buf.push_str(&format!("\t({}, {}): {}\n", statel, stater, viterbi));
                    for (bt, w) in backtraces {
                        match bt {
                            ChartEntry::Concat{mid_state, mid_range} => {
                                buf.push_str(&format!("\t\tCONCAT {} {} # {}\n", mid_state, mid_range, w));
                            },
                            ChartEntry::Initial{ label, .. } => {
                                buf.push_str(&format!("\t\tINIT {} # {}\n", int.find_value(*label).unwrap(), w));
                            },
                            ChartEntry::Wrap{ label, inner, weight } => {
                                buf.push_str(&format!("\t\tWRAP {} ({}, {})  {} # {}\n", int.find_value(*label).unwrap(), inner.left, inner.right, weight, w));
                            }
                        }
                    }
                }
            }
        }
        buf
    }
}

// #[derive(Clone, Debug)]
// pub enum ChartEntryWrapper<W> {
//     Unique(ChartEntry<W>, StateT, W),
//     Multiple(Vec<(ChartEntry<W>, StateT, W)>, W)
// }
// pub enum ChartEntryWrapperIterator<'a, W> {
//     Empty,
//     Unique(&'a ChartEntry<W>, &'a StateT, &'a W),
//     Multiple(Iter<'a, (ChartEntry<W>, StateT, W)>)
// }
// impl<'a, W> Iterator for ChartEntryWrapperIterator<'a, W> {
//     type Item = (&'a ChartEntry<W>, &'a StateT, &'a W);
//     fn next(&mut self) -> Option<Self::Item> {
//         use self::ChartEntryWrapperIterator::*;
        
//         if let &mut Empty = self {
//             return None;
//         }

//         if let &mut Multiple(ref mut v) = self {
//             return v.next().map(|&(ref a, ref b, ref c)| (a, b, c));
//         }

//         let mut x = ChartEntryWrapperIterator::Empty;
//         swap(&mut x, self);
        
//         if let Unique(ce, r, w) = x {
//             return Some((ce, r, w));
//         }
        
//         unreachable!()
//     }
// }
// impl<'a, W> IntoIterator for &'a ChartEntryWrapper<W> {
//     type Item = (&'a ChartEntry<W>, &'a StateT, &'a W);
//     type IntoIter = ChartEntryWrapperIterator<'a, W>;
//     fn into_iter(self) -> Self::IntoIter {
//         use self::ChartEntryWrapperIterator::*;
//         match self {
//             &ChartEntryWrapper::Unique(ref ce, ref r, ref w) => Unique(ce, r, w),
//             &ChartEntryWrapper::Multiple(ref v, _) => Multiple(v.iter())
//         }
//     }
// }

// impl<W> ChartEntryWrapper<W> {
//     pub fn viterbi(&self) -> &W {
//         match self {
//             &ChartEntryWrapper::Unique(_, _, ref w) => w,
//             &ChartEntryWrapper::Multiple(_, ref w) => w
//         }
//     }
    
//     pub fn add_entry(&mut self, entry: ChartEntry<W>, right: StateT, viterbi: W) where W: Copy + Ord {
//         use std::cmp::max;

//         if let &mut ChartEntryWrapper::Unique(e, r, w) = self {
//             *self = ChartEntryWrapper::Multiple(vec![(e, r, w), (entry, right, viterbi)], max(w, viterbi));
//             return;
//         }
//         if let &mut ChartEntryWrapper::Multiple(ref mut v, ref mut w) = self {
//             *w = max(*w, viterbi);
//             v.push((entry, right, viterbi));
//             return;
//         }
//         unreachable!()
//     }
// }

pub fn chart_index(i: usize, j: usize, n: usize) -> usize {
    ( n * (n+1) - (n - (j-i) + 1) * (n - (j-i) + 2) ) / 2 + i
}

impl<W> Chart for SemiCykChart<W>
where
    W: Ord + Copy
{
    type Weight = W;
    
    fn get_entries(&mut self, tr: TwinRange)
        -> Option<(IndexedChartEntry<W>, W, FnvUniqueHeap<IndexedChartEntry<W>, W>)>
    {
        let &mut SemiCykChart(ref mut v, n, _, _) = self;
        let mut backlinks: FnvUniqueHeap<IndexedChartEntry<W>, W>
            = v[chart_index(tr.range.left, tr.range.right, n)].1.remove(&(tr.state.left, tr.state.right))?
                                                              .into_iter().map(|(ce, w)| (ce.into(), w)).collect();
        let (fst, w) = backlinks.pop().unwrap();
        Some((fst, w, backlinks))
    }

    fn get_root(&self) -> TwinRange {
        let &SemiCykChart(_, n, left, right) = self;
        TwinRange{ state: TwinState{ left, right }, range: TwinState{ left: 0, right: n } }
    }
}

impl<W> SemiCykChart<W> {
    pub fn new(v: Vec<ChartCell<W>>, n: usize, tr: TwinRange) -> Self {
        let TwinRange{ state: TwinState{ left, right }, .. } = tr;
        SemiCykChart(v, n, left, right)
    }
}