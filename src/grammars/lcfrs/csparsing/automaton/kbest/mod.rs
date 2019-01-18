use super::{Automaton, Bracket, BracketContent, RangeT, StateT, TdBinary, TdUnary, TdNullary, TdBrackets, DenseChart, RuleIdT};
use fnv::FnvHashMap;
use unique_heap::FnvUniqueHeap;
use num_traits::Zero;
use std::{ ops::Mul, collections::hash_map::Entry, hash::Hash, cmp::min };

mod backtrace;
use self::backtrace::IndexedBacktrace;

/// implements necessary structures for the lazy k-best algorithm
pub struct ChartIterator<'a, W>
where
    W: Ord,
{
    chart: DenseChart<W>,
    binaries: &'a [Vec<TdBinary<W>>],
    unaries: &'a [Vec<TdUnary<W>>],
    nullaries: &'a [Vec<TdNullary<W>>],
    rules_to_brackets: &'a [TdBrackets],
    
    // caches already queried hyperpaths and holds the next candidates
    // for each span and state
    d: FnvHashMap<(RangeT, RangeT, StateT), (Vec<(IndexedBacktrace<W>, W)>, FnvUniqueHeap<IndexedBacktrace<W>, W>)>,
    
    // current k for root
    k: usize,
    n: usize,
    initial: StateT
}

impl<'a, W: Ord> ChartIterator<'a, W> {
    pub fn new<T: Eq + Hash>(chart: DenseChart<W>, automaton: &'a Automaton<T, W>) -> Self {
        let (n, states, beamwidth) = chart.get_meta();
        Self {
            chart,
            binaries: &automaton.3,
            unaries: &automaton.4,
            nullaries: &automaton.5,
            rules_to_brackets: &automaton.8,
            // we need at most `beam` entries for each span
            d: FnvHashMap::with_capacity_and_hasher(n * (n+1) * min(states, beamwidth) / 2, Default::default()),
            k: 0,
            n,
            initial: automaton.7
        }
    }
}

impl<'a, W> ChartIterator<'a, W>
where
    W: Mul<Output=W> + Ord + Copy + Zero,
{
    /// Computes the weight of an item recursively and checks the existence of all predecessors.
    fn weight(&mut self, i: RangeT, j: RangeT, ce: &IndexedBacktrace<W>) -> Option<W> {
        use self::IndexedBacktrace::*;
        
        match *ce {
            Nullary(_, weight) => Some(weight),
            Unary(_, q1, weight, index) => {
                let w = self.kth(i, j, q1, index as usize)?.1;
                Some(weight * w)
            },
            Binary(_, q1, m, q2, w, index1, index2) => {
                let w1 = self.kth(i, m, q1, index1 as usize)?.1;
                let w2 = self.kth(m, j, q2, index2 as usize)?.1;
                Some(w * w1 * w2)
            }
        }
    }

    /// extracts the backtraces for a spand and a constituents in a
    /// top-down approach
    fn backtraces(chart: &DenseChart<W>, binaries: &[Vec<TdBinary<W>>], unaries: &[Vec<TdUnary<W>>], nullaries: &[Vec<TdNullary<W>>], i: RangeT, j: RangeT, q: StateT) -> FnvUniqueHeap<IndexedBacktrace<W>, W> {
        let mut heap = FnvUniqueHeap::default();
        for &(r, q1, q2, w) in &binaries[q as usize] {
            for mid in (i+1)..j {
                if let Some(sws) = chart.get_weight(i, mid, q1).and_then(|lew| chart.get_weight(mid, j, q2).map(move |riw| lew * riw)) {
                    heap.push(IndexedBacktrace::Binary(r, q1, mid, q2, w, 0u32, 0u32), w * sws);
                }
            }
        }
        for &(r, q1, w) in &unaries[q as usize] {
            if let Some(w1) = chart.get_weight(i, j, q1) {
                heap.push(IndexedBacktrace::Unary(r, q1, w, 0u32), w1 * w);
            }
        }
        if j-i == 1 {
            for &(r, w) in &nullaries[q as usize] {
                // is is not correct in general, but ok for terminal-seperated rules
                heap.push(IndexedBacktrace::Nullary(r, w), w);
            }
        }
        heap
    }
    
    // Implementation of the lazy enumeration for hyperpaths in Better k-best Parsing.
    fn kth(&mut self, i: RangeT, j: RangeT, q: StateT, k: usize) -> Option<(IndexedBacktrace<W>, W)> {
        use std::cmp::Ordering;
        // initialize structures for span and state
        // todo skip fetch if vec_len > k
        let (mut vec_len, mut last_deriv, mut last_weight) = {
            let ChartIterator{ ref mut d, ref chart, ref binaries, ref unaries, ref nullaries, .. } = *self;
            match d.entry((i, j, q)) {
                Entry::Vacant(ve) => {
                    let mut bts = Self::backtraces(chart, binaries, unaries, nullaries, i, j, q);
                    if let Some((first, vit)) = bts.pop() {
                        let mut vec = Vec::with_capacity(bts.len() + 1);
                        vec.push((first, vit));
                        ve.insert((vec, bts));
                        (1, first, vit)
                    } else {
                        ve.insert((Vec::new(), FnvUniqueHeap::default()));
                        return None
                    }
                }, Entry::Occupied(oe) => {
                    let &(ref d, _) = oe.get();
                    if let Some(&(deriv, w)) = d.last() { (d.len(), deriv, w) }
                    else { return None }
                }
            }
        };

        while vec_len < (k + 1) {
            // there are at most 2 successors, so we collect them allocated in the stack
            // and at the same time to avoid getting the entry for the insertion trice
            let (wd1, wd2) = {
                let mut worse_derivs = last_deriv.iter().filter_map(|bt| Some((bt, self.weight(i, j, &bt)?)));
                (worse_derivs.next(), worse_derivs.next())
            };

            let &mut (ref mut derivs, ref mut candidates) = &mut self.d.get_mut(&(i, j, q)).unwrap();
            if let Some((bt, w)) = wd1 { candidates.push(bt, w); }
            if let Some((bt, w)) = wd2 { candidates.push(bt, w); }

            if candidates.is_empty() { break; }
            
            let last_deriv_w = candidates.pop().unwrap();
            derivs.push(last_deriv_w);
            last_deriv = last_deriv_w.0;
            last_weight = last_deriv_w.1;
            vec_len += 1;
        }

        match vec_len.cmp(&(k+1)) {
            Ordering::Equal => Some((last_deriv, last_weight)),
            Ordering::Less  => None,
            Ordering::Greater => Some(self.d[&(i, j, q)].0[k])
        }
    }

    // Reads the bracket word for a hyperpath.
    fn read(&mut self, i: RangeT, j: RangeT, ce: &IndexedBacktrace<W>) -> Vec<Bracket<BracketContent>> {
        use self::IndexedBacktrace::*;

        match *ce {
            Binary(rid, ls, m, rs, _, lk, rk) => {
                let ce1 = self.kth(i, m, ls, lk as usize).unwrap().0;
                let ce2 = self.kth(m, j, rs, rk as usize).unwrap().0;

                let (ob, lb, rb) = self.rules_to_brackets[rid as usize];
                let mut additional_elements = 2 + if ob.is_ignore() { 0 } else { 2 } + if lb.is_ignore() { 0 } else { 2 };
                
                let mut w1 = self.read(i, m, &ce1);
                let w2 = self.read(m, j, &ce2);
                w1.reserve(w2.len() + additional_elements);
                if !lb.is_ignore() {
                    w1.insert(0, Bracket::Open(lb));
                    w1.push(Bracket::Close(lb));
                }
                w1.push(Bracket::Open(rb));
                w1.extend(w2);
                w1.push(Bracket::Close(rb));
                if !ob.is_ignore() {
                    w1.insert(0, Bracket::Open(ob));
                    w1.push(Bracket::Open(ob));
                }
                
                w1
            },
            Unary(rid, q, _, k) => {
                let ice = self.kth(i, j, q, k as usize).unwrap().0;
                let mut w = self.read(i, j, &ice);
                w.reserve(4);
                let (ob, ib, _) = self.rules_to_brackets[rid as usize];
                
                w.insert(0, Bracket::Open(ob));
                w.insert(1, Bracket::Open(ib));
                w.push(Bracket::Close(ib));
                w.push(Bracket::Close(ob));
                w
            },
            Nullary(rid, _) => {
                let (ob, ib, _) = self.rules_to_brackets[rid as usize];
                vec![ Bracket::Open(ob)
                    , Bracket::Open(ib)
                    , Bracket::Close(ib)
                    , Bracket::Close(ob)
                    ]
            }
        }
    }
}

impl<'a, W: Ord + Copy + Mul<Output=W> + Zero> Iterator for ChartIterator<'a, W> {
    type Item = Vec<Bracket<BracketContent>>;
    fn next(&mut self) -> Option<Self::Item> {
        let &mut ChartIterator{ initial, n, k, .. } = self;
        self.k += 1;

        self.kth(0u8, n as u8, initial, k)
            .map(|(backtrace, _)| self.read(0u8, n as u8, &backtrace))
    }
}