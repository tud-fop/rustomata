use super::{Chart, ChartEntry, IndexedChartEntry, TwinRange};
use unique_heap::FnvUniqueHeap;
use fnv::FnvHashMap;

/// A `Chart` contains a compact representation of all Dyck words that we
/// extracted from a `CykAutomaton`.
#[derive(Clone, Debug)]
pub struct SparseChart<W>(
    // Contains the actual chart, a relation between `TwinRange`s and
    // backtraces.
    FnvHashMap<TwinRange, Vec<(ChartEntry<W>, W)>>,
    // The root entry; it's the same as the initial/final state range of the
    // `CykAutomaton`. 
    TwinRange,
);

impl<W> Chart for SparseChart<W>
where
    W: Ord + Copy
{
    type Weight = W;
    
    fn get_entries(&mut self, tr: TwinRange)
        -> Option<(IndexedChartEntry<W>, W, FnvUniqueHeap<IndexedChartEntry<W>, W>)>
    {
        let mut v = self.0.remove(&tr)?;
        let (fst, w) = v.remove(0);
        let h = v.into_iter().map(|(ce, w)| (ce.into(), w)).collect();
        Some((fst.into(), w, h))
    }

    fn get_root(&self) -> TwinRange {
        self.1
    }
}

impl<W> SparseChart<W> {
    pub fn new(m: FnvHashMap<TwinRange, Vec<(ChartEntry<W>, W)>>, tr: TwinRange) -> Self {
        SparseChart(m, tr)
    }
}

use std::fmt::Display;
use integeriser::{Integeriser, HashIntegeriser};
use std::rc::Rc;
use std::hash::Hash;
use std::cmp::max;
impl<W> SparseChart<W> {
    pub fn to_pretty_string<T>(&self, int: &Rc<HashIntegeriser<T>>) -> String
    where
        W: Display,
        T: Display + Eq + Hash + Clone
    {
        let mut buf = String::new();
        let mut cyk_chart: FnvHashMap<(usize, usize), FnvHashMap<(usize, usize), &Vec<(ChartEntry<W>, W)>>>
            = FnvHashMap::default();
        let mut n = 0;
        for (tr, v) in &self.0 {
            cyk_chart.entry((tr.range.left, tr.range.right)).or_default()
                     .insert((tr.state.left, tr.state.right), v);
            n = max(n, tr.range.right);
        }

        for range in 1..=n {
            for l in 0..=(n - range) {
                let r = l + range;
                buf.push_str(&format!("{}, {}:\n", l, r));
                for (&(statel, stater), backtraces) in cyk_chart.get(&(l, r)).into_iter().flatten() {
                    let &(_, ref viterbi) = &backtraces[0];
                    buf.push_str(&format!("\t({}, {}): {}\n", statel, stater, viterbi));
                    for (bt, w) in *backtraces {
                        match bt {
                            ChartEntry::Concat{mid_state, mid_range} => {
                                buf.push_str(&format!("\t\tCONCAT {} {} # {}\n", mid_state, mid_range, w));
                            },
                            ChartEntry::Initial{ label, .. } => {
                                buf.push_str(&format!("\t\tINIT {} # {}\n", int.find_value(*label).unwrap(), w));
                            },
                            ChartEntry::Wrap{ label, inner, weight } => {
                                buf.push_str(&format!("\t\tWRAP {} ({}, {}) {} # {}\n", int.find_value(*label).unwrap(), inner.left, inner.right, weight, w));
                            }
                        }
                    }
                }
            }
        }
        buf
    }
}