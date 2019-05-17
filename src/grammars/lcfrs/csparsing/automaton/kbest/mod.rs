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
    rulefilter: Vec<bool>,
    
    // caches already queried hyperpaths and holds the next candidates
    // for each span and state
    d: FnvHashMap<(RangeT, RangeT, StateT), (Vec<(IndexedBacktrace<W>, W)>, FnvUniqueHeap<IndexedBacktrace<W>, W>)>,
    
    // current k for root
    k: usize,
    n: usize,
    initial: StateT
}

impl<'a, W: Ord> ChartIterator<'a, W> {
    pub fn new<T: Eq + Hash>(chart: DenseChart<W>, automaton: &'a Automaton<T, W>, rulefilter: Vec<bool>) -> Self {
        let (n, states, beamwidth) = chart.get_meta();
        Self {
            chart,
            binaries: &automaton.3,
            unaries: &automaton.4,
            nullaries: &automaton.5,
            rules_to_brackets: &automaton.8,
            rulefilter,
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
    fn backtraces(chart: &DenseChart<W>, binaries: &[Vec<TdBinary<W>>], unaries: &[Vec<TdUnary<W>>], nullaries: &[Vec<TdNullary<W>>], filter: &[bool], i: RangeT, j: RangeT, q: StateT) -> FnvUniqueHeap<IndexedBacktrace<W>, W> {
        let mut heap = FnvUniqueHeap::default();
        for &(r, q1, q2, w) in binaries[q as usize].iter().filter(|&(r, _, _, _)| filter[*r as usize]) {
            for mid in (i+1)..j {
                if let Some(sws) = chart.get_weight(i, mid, q1).and_then(|lew| chart.get_weight(mid, j, q2).map(move |riw| lew * riw)) {
                    heap.push(IndexedBacktrace::Binary(r, q1, mid, q2, w, 0u32, 0u32), w * sws);
                }
            }
        }
        for &(r, q1, w) in unaries[q as usize].iter().filter(|&(r, _, _)| filter[*r as usize]) {
            if let Some(w1) = chart.get_weight(i, j, q1) {
                heap.push(IndexedBacktrace::Unary(r, q1, w, 0u32), w1 * w);
            }
        }
        if j-i == 1 {
            for &(r, w) in nullaries[q as usize].iter().filter(|&(r, _)| filter[*r as usize]) {
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
            let ChartIterator{ ref mut d, ref chart, ref binaries, ref unaries, ref nullaries, ref rulefilter, .. } = *self;
            match d.entry((i, j, q)) {
                Entry::Vacant(ve) => {
                    let mut bts = Self::backtraces(chart, binaries, unaries, nullaries, &rulefilter, i, j, q);
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
                let additional_elements = 2 + if ob.is_ignore() { 0 } else { 2 } + if lb.is_ignore() { 0 } else { 2 };
                
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
                    w1.push(Bracket::Close(ob));
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

#[cfg(test)]
mod test {
    use super::*;
    use super::super::*;
    use log_domain::LogDomain;
        extern crate bincode;
        extern crate flate2;

    pub fn example_automaton() -> Automaton<String, LogDomain<f64>> {
        use crate::grammars::lcfrs::Lcfrs;
        let g: Lcfrs<String, String, LogDomain<f64>>
            = "initial: [S]\n\n
               S → [[T a]] () # 0.7\n
               S → [[Var 0 0]] (S) # 0.3".parse().unwrap();
        Automaton::from_grammar(g.rules.iter().enumerate().map(|(i, r)| (i as u32, r)), g.init)
    }
    
    #[test]
    fn kth() {
        let zero = LogDomain::zero();
        let w1 = LogDomain::new(0.7).unwrap();
        let w2 = LogDomain::new(0.3).unwrap();

        let automaton = example_automaton();
        let estimates = SxOutside::from_automaton(&automaton, 0);
        let chart = automaton.fill_chart(&[String::from("a")], 1, zero, &estimates, &vec![true, true]);
        let mut it = ChartIterator::new(chart, &automaton, vec![true, true]);

        assert_eq!(
            it.kth(0, 1, 0, 0),
            Some((IndexedBacktrace::Nullary(0, w1), w1))
        );

        assert_eq!(
            it.kth(0, 1, 0, 1),
            Some((IndexedBacktrace::Unary(1, 0, w2, 0), w1 * w2))
        );

        assert_eq!(
            it.kth(0, 1, 0, 2),
            Some((IndexedBacktrace::Unary(1, 0, w2, 1), w1 * w2 * w2))
        );

        assert_eq!(
            it.kth(0, 1, 0, 3),
            Some((IndexedBacktrace::Unary(1, 0, w2, 2), w1 * w2 * w2 * w2))
        );

    }

    #[test]
    fn structure() {
        let zero = LogDomain::zero();
        let w1 = LogDomain::new(0.7).unwrap();
        let w2 = LogDomain::new(0.3).unwrap();

        let automaton = example_automaton();
        let estimates = SxOutside::from_automaton(&automaton, 0);
        let chart = automaton.fill_chart(&[String::from("a")], 1, zero, &estimates, &[true, true]);
        let mut it = ChartIterator::new(chart, &automaton, vec![true, true]);

        assert!(it.d.is_empty());
        assert_eq!(it.k, 0);

        assert!(it.next().is_some());
        assert_eq!(it.d[&(0, 1, 0)].0, vec![(IndexedBacktrace::Nullary(0, w1), w1)]);
        assert_eq!(it.d[&(0, 1, 0)].1.clone().into_sorted_vec(), vec![(w1 * w2, IndexedBacktrace::Unary(1, 0, w2, 0))]);

        assert!(it.next().is_some());
        assert_eq!(it.d[&(0, 1, 0)].0, vec![(IndexedBacktrace::Nullary(0, w1), w1), (IndexedBacktrace::Unary(1, 0, w2, 0), w1 * w2)]);
        assert!(it.d[&(0, 1, 0)].1.is_empty());

        assert!(it.next().is_some());
    }

    #[test]
    fn elements() {
        let zero = LogDomain::zero();
        let automaton = example_automaton();
        let estimates = SxOutside::from_automaton(&automaton, 0);
        let it = ChartIterator::new(automaton.fill_chart(&[String::from("a")], 1, zero, &estimates, &[true, true]), &automaton, vec![true, true]);
        
        assert_eq!(
            it.take(10).count(),
            10
        );

        let it = ChartIterator::new(automaton.fill_chart(&[String::from("a")], 1, zero, &estimates, &[true, true]), &automaton, vec![true, true]);
        assert_eq!(
            it.take(4).collect::<Vec<_>>(),
            vec![
                vec![
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Terminal),
                    Bracket::Close(BracketContent::Terminal),
                    Bracket::Close(BracketContent::Component(0, 0)),
                ],
                vec![
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Variable(1, 0, 0)),
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Terminal),
                    Bracket::Close(BracketContent::Terminal),
                    Bracket::Close(BracketContent::Component(0, 0)),
                    Bracket::Close(BracketContent::Variable(1, 0, 0)),
                    Bracket::Close(BracketContent::Component(1, 0)),
                ],
                vec![
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Variable(1, 0, 0)),
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Variable(1, 0, 0)),
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Terminal),
                    Bracket::Close(BracketContent::Terminal),
                    Bracket::Close(BracketContent::Component(0, 0)),
                    Bracket::Close(BracketContent::Variable(1, 0, 0)),
                    Bracket::Close(BracketContent::Component(1, 0)),
                    Bracket::Close(BracketContent::Variable(1, 0, 0)),
                    Bracket::Close(BracketContent::Component(1, 0)),
                ],
                vec![
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Variable(1, 0, 0)),
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Variable(1, 0, 0)),
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Variable(1, 0, 0)),
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Terminal),
                    Bracket::Close(BracketContent::Terminal),
                    Bracket::Close(BracketContent::Component(0, 0)),
                    Bracket::Close(BracketContent::Variable(1, 0, 0)),
                    Bracket::Close(BracketContent::Component(1, 0)),
                    Bracket::Close(BracketContent::Variable(1, 0, 0)),
                    Bracket::Close(BracketContent::Component(1, 0)),
                    Bracket::Close(BracketContent::Variable(1, 0, 0)),
                    Bracket::Close(BracketContent::Component(1, 0)),
                ],
            ]
        )
    }

    #[test]
    fn kth2 () {
        let zero = LogDomain::zero();
        let automaton = example_automaton2();
        let estimates = SxOutside::from_automaton(&automaton, 0);
        let filter = vec![true; 15];
        let words: Vec<String> = vec!["a", "c", "b", "b", "d"].into_iter().map(|s| s.to_owned()).collect();
        let chart = automaton.fill_chart(&words, 10, zero, &estimates, &filter);

        assert!(chart.get_weight(0, 5, 0).is_some());
        
        let mut it = ChartIterator::new(chart, &automaton, filter);

        for i in 1..10 {
            assert!(it.kth(0, 5, 0, i).is_none(), "failed at {}", i);
        }
    }

    #[test]
    fn elements2 () {
        let zero = LogDomain::zero();
        let automaton = example_automaton2();
        let estimates = SxOutside::from_automaton(&automaton, 0);
        let words: Vec<String> = vec!["a", "c", "b", "b", "d"].into_iter().map(|s| s.to_owned()).collect();
        let filter = vec![true; 15];
        let chart = automaton.fill_chart(&words, 10, zero, &estimates, &filter);

        assert_eq!(
            ChartIterator::new(chart, &automaton, filter.clone()).take(10).count(),
            1
        );

        let chart = automaton.fill_chart(&words, 10, zero, &estimates, &filter);
        let it = ChartIterator::new(chart, &automaton, filter);
        
        let some_words = it.collect::<Vec<_>>();
        let first = example_words2();

        assert_eq!(
            some_words,
            first
        );
    }

    fn example_automaton2 () -> Automaton<String, LogDomain<f64>> {
        use crate::grammars::lcfrs::Lcfrs;
        let Lcfrs{ rules, init }: Lcfrs<String, String, LogDomain<f64>>
                    = "initial: [S]\n\n
                       S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B) # 1\n
                       A → [[Var 0 0, Var 1 0], [Var 0 1, Var 2 0]] (A, W, X) # 0.4\n
                       A → [[Var 0 0], [Var 1 0]] (W, X) # 0.6\n
                       B → [[Var 0 0, Var 1 0], [Var 0 1, Var 2 0]] (B, Y, Z) # 0.3\n
                       B → [[Var 0 0], [Var 1 0]] (Y, Z) # 0.7\n
                       W → [[T a]] () # 1\n
                       X → [[T b]] () # 1\n
                       Y → [[T c]] () # 1\n
                       Z → [[T d]] () # 1".parse().unwrap();
        Automaton::from_grammar(rules.iter().enumerate().map(|(i, r)| (i as u32, r)), init)
    }

    fn example_words2 () -> Vec<Vec<Bracket<BracketContent>>> {
        vec![
            vec![
                Bracket::Open(BracketContent::Component(0, 0)),
                Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    
                    Bracket::Open(BracketContent::Component(2, 0)),
                    Bracket::Open(BracketContent::Variable(2, 0, 0)),
                        Bracket::Open(BracketContent::Component(5, 0)),
                        Bracket::Open(BracketContent::Terminal),
                        Bracket::Close(BracketContent::Terminal),
                        Bracket::Close(BracketContent::Component(5, 0)),
                    Bracket::Close(BracketContent::Variable(2, 0, 0)),
                    Bracket::Close(BracketContent::Component(2, 0)),

                Bracket::Close(BracketContent::Variable(0, 0, 0)),
                Bracket::Open(BracketContent::Variable(0, 1, 0)),

                Bracket::Open(BracketContent::Component(4, 0)),
                Bracket::Open(BracketContent::Variable(4, 0, 0)),
                Bracket::Open(BracketContent::Component(7, 0)),
                Bracket::Open(BracketContent::Terminal),
                Bracket::Close(BracketContent::Terminal),
                Bracket::Close(BracketContent::Component(7, 0)),
                Bracket::Close(BracketContent::Variable(4, 0, 0)),
                Bracket::Close(BracketContent::Component(4, 0)),

                Bracket::Close(BracketContent::Variable(0, 1, 0)),
                Bracket::Open(BracketContent::Variable(0, 0, 1)),

                    Bracket::Open(BracketContent::Component(1, 1)),
                    Bracket::Open(BracketContent::Variable(1, 0, 1)),

                        Bracket::Open(BracketContent::Component(2, 1)),
                        Bracket::Open(BracketContent::Variable(2, 1, 0)),
                            Bracket::Open(BracketContent::Component(6, 0)),
                            Bracket::Open(BracketContent::Terminal),
                            Bracket::Close(BracketContent::Terminal),
                            Bracket::Close(BracketContent::Component(6, 0)),
                        Bracket::Close(BracketContent::Variable(2, 1, 0)),
                        Bracket::Close(BracketContent::Component(2, 1)),

                    Bracket::Close(BracketContent::Variable(1, 0, 1)),
                    Bracket::Open(BracketContent::Variable(1, 2, 0)),

                        Bracket::Open(BracketContent::Component(6, 0)),
                        Bracket::Open(BracketContent::Terminal),
                        Bracket::Close(BracketContent::Terminal),
                        Bracket::Close(BracketContent::Component(6, 0)),
                                 
                    Bracket::Close(BracketContent::Variable(1, 2, 0)),
                    Bracket::Close(BracketContent::Component(1, 1)),

                Bracket::Close(BracketContent::Variable(0, 0, 1)),
                Bracket::Open(BracketContent::Variable(0, 1, 1)),

                Bracket::Open(BracketContent::Component(4, 1)),
                Bracket::Open(BracketContent::Variable(4, 1, 0)),
                Bracket::Open(BracketContent::Component(8, 0)),
                Bracket::Open(BracketContent::Terminal),
                Bracket::Close(BracketContent::Terminal),
                Bracket::Close(BracketContent::Component(8, 0)),
                Bracket::Close(BracketContent::Variable(4, 1, 0)),
                Bracket::Close(BracketContent::Component(4, 1)),
                
                Bracket::Close(BracketContent::Variable(0, 1, 1)),
                Bracket::Close(BracketContent::Component(0, 0)),
            ]
        ]
    }

    // #[test]
    // fn example_automaton3 () {
    //     use self::flate2::read;
    //     use std::fs::File;
    //     use grammars::lcfrs::csparsing::CSRepresentation;

    //     let csfile = File::open("example-opt.cs").unwrap();
    //     let csrep: CSRepresentation<String, String, LogDomain<f64>> =
    //             bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite)
    //                 .unwrap();
        
    //     let automaton = csrep.generator;
    //     let estimate = csrep.estimates;

    //     let words: Vec<String> = vec!["ADJD", "ADV", "$,", "KOUS", "ADV", "PIS", "PROAV", "VVINF", "VMFIN", "$."].into_iter().map(|s| s.to_owned()).collect();
    //     let mut it = ChartIterator::new(automaton.fill_chart(&words, automaton.1.len(), LogDomain::zero(), &estimate), &automaton);
    //     it.next();
    //     std::dbg!(it.d);
    // }
}