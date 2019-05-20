use super::{RangeT, StateT};
use num_traits::Zero;
use std::mem::zeroed;
use std::ops::AddAssign;

/// A chart for cfg parsing.
#[derive(Debug, Clone)]
pub struct DenseChart<W>(
    Vec<u16>,         // no. of saved nonterminals per span
    Vec<(StateT, W)>, // nonterminals with viterbi weigh per span (max <beam> entries)
    Vec<W>,           // viterbi weight per span and nonterminal
    usize,            // n
    usize,            // states
    u16,              // max no. of constituents per span
);

pub fn chart_size(n: usize) -> usize {
    n * (n + 1) / 2
}
pub fn chart_size_with_states(n: usize, states: usize) -> usize {
    n * (n + 1) * states / 2
}

pub fn index(i: RangeT, j: RangeT, n: usize) -> usize {
    let (i, j) = (i as usize, j as usize);
    (n * (n + 1) - (n - (j - i) + 1) * (n - (j - i) + 2)) / 2 + i
}
pub fn index_with_state(i: RangeT, j: RangeT, q: StateT, n: usize, states: usize) -> usize {
    let (i, j, q) = (i as usize, j as usize, q as usize);
    ((n * (n + 1) - (n - (j - i) + 1) * (n - (j - i) + 2)) / 2 + i) * states + q
}

impl<W: Copy> DenseChart<W> {
    /// Allocates the whole space needed for the chart.
    /// All data structures are initialized with zeroes.
    pub fn new(n: usize, states: usize, bt_per_cell: usize) -> Self
    where
        W: Zero,
    {
        assert!(bt_per_cell <= u16::max_value() as usize);
        DenseChart(
            vec![unsafe { zeroed() }; chart_size(n)],
            vec![unsafe { zeroed() }; chart_size(n) * bt_per_cell],
            vec![W::zero(); chart_size_with_states(n, states)],
            n,
            states,
            bt_per_cell as u16,
        )
    }

    /// Adds a constituent with viterbi weight to a span.
    pub fn add_entry(&mut self, i: RangeT, j: RangeT, state: StateT, weight: W) {
        let tri_index = index(i, j, self.3);
        let nts = &mut self.0[tri_index];
        if *nts < self.5 {
            self.1[tri_index * self.5 as usize + *nts as usize] = (state, weight);
            self.2[tri_index * self.4 + state as usize] = weight;
            AddAssign::add_assign(nts, 1);
        }
    }

    /// Gets weight for specific constituent and span.
    pub fn get_weight(&self, i: RangeT, j: RangeT, q: StateT) -> Option<W>
    where
        W: PartialEq + Zero,
    {
        let w = self.2[index_with_state(i, j, q, self.3, self.4)];
        if w == W::zero() {
            None
        } else {
            Some(w)
        }
    }

    pub fn get_best(&self, i: RangeT, j: RangeT) -> Option<(StateT, W)>
    where
        W: Zero + PartialEq,
    {
        let index = index(i, j, self.3) * self.5 as usize;
        let (state, w) = self.1[index];
        if w == W::zero() {
            None
        } else {
            Some((state, w))
        }
    }
}

impl<W> DenseChart<W> {
    /// Gives information about the size of the chart. Returns n, the state
    /// count and the beam width.
    pub fn get_meta(&self) -> (usize, usize, usize) {
        (self.3, self.4, self.5 as usize)
    }

    /// Iterates all constituents for a span.
    pub fn iterate_nont<'a>(
        &'a self,
        i: RangeT,
        j: RangeT,
    ) -> impl 'a + Iterator<Item = &'a (StateT, W)> {
        let tri_index = index(i, j, self.3);
        let first_index = tri_index * self.5 as usize;
        let last_index = first_index + self.0[tri_index] as usize;
        self.1[first_index..last_index].iter()
    }
}
