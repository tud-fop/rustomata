use super::{ChartEntry, chart_entry::IndexedChartEntry, TwinRange};
use unique_heap::FnvUniqueHeap;

pub mod semicyk;
pub mod sparse;

pub trait Chart {
    type Weight: Ord;

    fn get_entries(&mut self, _: TwinRange)
        -> Option<(IndexedChartEntry<Self::Weight>, Self::Weight, FnvUniqueHeap<IndexedChartEntry<Self::Weight>, Self::Weight>)>;
    fn get_root(&self) -> TwinRange;
}