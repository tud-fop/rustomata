/// This module implements the better k-best parsing algorithm
/// described by Huang and Chiang in 2005 in Algorithm 3.

use super::chart_entry::IndexedChartEntry;
use super::twin_state::{TwinRange};
use super::Chart;
use dyck::Bracket;
use util::reverse::Reverse;

use std::{ ops::Mul, hash::Hash };
use fnv::FnvHashMap;
use integeriser::Integeriser;

extern crate unique_heap;
use self::unique_heap::FnvUniqueHeap;

/// Stores the information for the lazy enumeration of hyperpaths.
pub struct ChartIterator<T, W>
where
    T: Hash + Eq,
    W: Ord
{
    chart: Chart<T, W>,
    
    // caches already queried hyperpaths
    d: FnvHashMap<TwinRange, Vec<(IndexedChartEntry<W>, W)>>,
    // stores candidates for next elements in d
    c: FnvHashMap<TwinRange, FnvUniqueHeap<IndexedChartEntry<W>, Reverse<W>>>,
    
    // current k for root
    k: usize
}

impl<T, W> ChartIterator<T, W>
where
    T: Eq + Hash,
    W: Mul<Output=W> + Ord + Copy
{
    pub fn new(chart: Chart<T, W>) -> Self {
        ChartIterator {
            chart,
            d: FnvHashMap::default(),
            c: FnvHashMap::default(),
            k: 0
        }
    }

    /// Computes the weight of an item recursively and checks the existence of all predecessors.
    fn weight(&mut self, origin: TwinRange, ce: &IndexedChartEntry<W>) -> Option<W> {
        use self::IndexedChartEntry::*;
        
        match ce {
            &Initial{ weight, .. } => Some(weight),
            &Wrap{ weight, inner, index, .. } => {
                let w = self.kth(origin.apply_state(inner), index)?.1;
                Some(weight * w)
            },
            &Concat{ mid_range, mid_state, index_left, index_right } => {
                let (left, right) = origin.split(mid_state, mid_range);
                let w1 = self.kth(left, index_left)?.1;
                let w2 = self.kth(right, index_right)?.1;
                Some(w1 * w2)
            }
        }
    }
    
    // Implementation of the lazy enumeration for hyperpaths in Better k-best Parsing.
    fn kth(&mut self, state: TwinRange, k: usize) -> Option<(IndexedChartEntry<W>, W)> {
        if let Some(deriv) = self.d.get(&state).and_then(|v| v.get(k)) {
            return Some(*deriv)
        }

        if self.c.get(&state).is_none() {
            self.c.insert(
                state, 
                self.chart.0.get(&state).into_iter().flat_map(|v| v).map(|&(ce, w)| (ce.into(), w.into())).collect()
            );
            self.d.insert(
                state,
                self.c.get_mut(&state).and_then(|h| h.pop()).map(|(ce, w)| (ce, w.unwrap())).into_iter().collect()
            );
        }

        while self.d.get(&state).unwrap().len() < (k + 1) {
            if let Some((ce, _)) = self.d.get(&state).unwrap().last().map(|t| t.clone()) {
                for ce_ in ce.successors() {
                    if let Some(weight) = self.weight(state, &ce_) {
                        self.c.get_mut(&state).unwrap().push(ce_, weight.into());
                    }
                }
            }

            if self.c.get(&state).unwrap().is_empty() {
                break;
            }
            
            self.d.get_mut(&state).unwrap().push(
                self.c.get_mut(&state).unwrap().pop().map(|(ce, w)| (ce, w.unwrap())).unwrap()
            )

        }

        self.d.get(&state).and_then(|v| v.get(k)).map(|t| *t)
    }

    // Reads the bracket word for a hyperpath.
    fn read(&mut self, origin: TwinRange, ce: &IndexedChartEntry<W>) -> Vec<Bracket<T>>
    where
        T: Clone
    {
        use self::IndexedChartEntry::*;

        match *ce {
            Concat{ mid_range, mid_state, index_left, index_right } => {
                let (left, right) = origin.split(mid_state, mid_range);
                let (ce1, _) = self.kth(left, index_left).unwrap();
                let (ce2, _) = self.kth(right, index_right).unwrap();
                
                let mut w = self.read(left, &ce1);
                w.extend(self.read(right, &ce2));
                w
            },
            Wrap{ label, index, inner, .. } => {
                let (ce_, _) = self.kth(origin.apply_state(inner), index).unwrap();
                let mut w = self.read(origin.apply_state(inner), &ce_);
                let t = self.chart.2.find_value(label).unwrap();
                // let t = self.fsa.bracket_i.find_value(ti).unwrap();
                
                w.insert(0, Bracket::Open(t.clone()));
                w.push(Bracket::Close(t.clone()));
                w
            }
            Initial{ label, .. } => {
                // self.fsa.terminal_i.find_value(wi).unwrap().clone()
                let t = self.chart.2.find_value(label).unwrap();
                vec![Bracket::Open(t.clone()), Bracket::Close(t.clone())]
            }
        }
    }
}

impl<T, W> Iterator for ChartIterator<T, W>
where
    T: Eq + Hash + Clone,
    W: Ord + Mul<Output=W> + Copy
{
    type Item = Vec<Bracket<T>>;
    fn next(&mut self) -> Option<Self::Item> {
        let k = self.k;
        self.k += 1;
        let root = self.chart.1;

        self.kth(root, k).map(
            |(ce, _)| self.read(root, &ce)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::*;
    use num_traits::One;
    use log_domain::LogDomain;

    pub fn example_automaton() -> CykAutomaton<BracketContent<&'static str>, Reverse<LogDomain<f64>>> {
        let mut integeriser = HashIntegeriser::new();
        for t in vec![BracketContent::Terminal("a"), BracketContent::Component(1, 0), BracketContent::Variable(0, 0, 0), BracketContent::Component(0, 0)] {
            integeriser.integerise(t);
        }
        let initials = vec![(TwinRange{ state: TwinState{ left: 0, right: 1 }, range: TwinState{ left: 0, right: 1 } }, 0, LogDomain::one().into())];
        let twin_arcs = vec![
            (TwinState{ left: 0, right: 1 }, vec![TwinArc{ left: 2, right: 3, label: 1, weight: LogDomain::new(0.75).unwrap().into() }]),
            (TwinState{ left: 2, right: 3 }, vec![TwinArc{ left: 4, right: 5, label: 2, weight: LogDomain::one().into() }]),
            (TwinState{ left: 4, right: 5 }, vec![TwinArc{ left: 2, right: 3, label: 3, weight: LogDomain::new(0.25).unwrap().into() }]),
        ].into_iter().collect();
        let finals = TwinRange{ state: TwinState{ left: 2, right: 3 }, range: TwinState{ left: 0, right: 1 }};

        CykAutomaton{ initials, twin_arcs, finals, integeriser: Rc::new(integeriser) }
    }
    
    #[test]
    fn kth() {
        let w1 = LogDomain::new(0.25).unwrap().into();
        let w2 = LogDomain::new(0.75).unwrap().into();
        
        use self::IndexedChartEntry::*;
        let chart = example_automaton().fill_chart();

        let t1 = chart.2.find_key(&BracketContent::Variable(0, 0, 0)).unwrap();
        let t2 = chart.2.find_key(&BracketContent::Component(0, 0)).unwrap();
        let t3 = chart.2.find_key(&BracketContent::Component(1, 0)).unwrap();
        
        let mut it = chart.into_iter();

        let state1 = it.chart.1;
        let state2 = TwinRange{ state: TwinState{ left: 4, right: 5 }, range: TwinState{ left: 0, right: 1 } };

        assert_eq!(
            it.kth(state1, 0),
            Some((Wrap{ label: t3, inner: TwinState{ left: 0, right: 1 }, weight: w2, index: 0 }, w2))
        );

        assert_eq!(
            it.kth(state2, 0),
            Some((Wrap{ label: t1, inner: TwinState{ left: 2, right: 3 }, index: 0, weight: w2 }, w2))
        );

        assert_eq!(
            it.kth(state1, 1),
            Some((Wrap{ label: t2, inner: TwinState{ left: 4, right: 5 }, weight: w1, index: 0 }, w2 * w1))
        );

        assert_eq!(
            it.kth(state1, 2),
            Some((Wrap{ label: t2, inner: TwinState{ left: 4, right: 5 }, weight: w1, index: 1 }, w2 * w1 * w1))
        );

    }

    #[test]
    fn structure() {
        use self::IndexedChartEntry::*;

        let one: Reverse<LogDomain<f64>> = LogDomain::one().into();
        let w1: Reverse<LogDomain<f64>> = LogDomain::new(0.25).unwrap().into();
        let w2 = LogDomain::new(0.75).unwrap().into();

        let mut it = example_automaton().fill_chart().into_iter();

        let t1 = it.chart.2.find_key(&BracketContent::Component(0, 0)).unwrap();
        let t2 = it.chart.2.find_key(&BracketContent::Variable(0, 0, 0)).unwrap();
        let t3 = it.chart.2.find_key(&BracketContent::Component(1, 0)).unwrap();
        let ta = it.chart.2.find_key(&BracketContent::Terminal("a")).unwrap();

        assert!(it.d.is_empty());
        assert!(it.c.is_empty());
        assert_eq!(it.k, 0);

        assert!(it.next().is_some());

        let state1 = it.chart.1;
        let state2 = TwinRange{ state: TwinState{ left: 4, right: 5 }, range: TwinState{ left: 0, right: 1 } };
        let state3 = TwinRange{ state: TwinState{ left: 0, right: 1 }, range: TwinState{ left: 0, right: 1 } };

        assert_eq!(
            it.d,
            vec![
                ( state3
                , vec![(Initial{ label: ta, weight: one }, one)]
                ),
                ( state1 
                , vec![(Wrap{ label: t3, inner: TwinState{ left: 0, right: 1 }, weight: w2, index: 0 }, w2)]
                )
            ].into_iter().collect()
        );
    
        let w_: Reverse<LogDomain<f64>> = w1 * w2;
        let w: Reverse<Reverse<LogDomain<f64>>> = w_.into();
        assert_eq!(
            it.c.iter().map(|(k, h)| (k.clone(), h.clone().into_sorted_vec())).collect::<Vec<_>>(),
            vec![
                ( state1
                , vec![(w, Wrap{ label: t1, inner: TwinState{ left: 4, right: 5 }, weight: w1, index: 0 })]
                ),
                (state3, vec![])
            ]
        );

        assert!(it.next().is_some());

        assert!(it.c.get(&state1).expect("empty heap").is_empty());
        assert!(it.c.get(&state2).expect("empty heap").is_empty());
        
        assert_eq!(
            it.d,
            vec![
                ( state3
                , vec![(Initial{ label: ta, weight: one }, one)]
                ),
                ( state1
                , vec![
                    (Wrap{ label: t3, inner: TwinState{ left: 0, right: 1 }, weight: w2, index: 0 }, w2),
                    (Wrap{ label: t1, inner: TwinState{ left: 4, right: 5 }, weight: w1, index: 0 }, w2 * w1)
                  ]
                ),
                ( state2
                , vec![
                    (Wrap{ label: t2, inner: TwinState{ left: 2, right: 3 }, index: 0, weight: w2 }, w2),
                  ]
                )
            ].into_iter().collect()
        );

        assert!(it.next().is_some());
    }

    #[test]
    fn elements() {
        let chart = example_automaton().fill_chart();

        assert_eq!(
            chart.clone().into_iter().take(10).count(),
            10
        );

        assert_eq!(
            chart.into_iter().take(4).collect::<Vec<_>>(),
            vec![
                vec![
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a")),
                    Bracket::Close(BracketContent::Terminal("a")),
                    Bracket::Close(BracketContent::Component(1, 0)),
                ],
                vec![
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a")),
                    Bracket::Close(BracketContent::Terminal("a")),
                    Bracket::Close(BracketContent::Component(1, 0)),
                    Bracket::Close(BracketContent::Variable(0, 0, 0)),
                    Bracket::Close(BracketContent::Component(0, 0)),
                ],
                vec![
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a")),
                    Bracket::Close(BracketContent::Terminal("a")),
                    Bracket::Close(BracketContent::Component(1, 0)),
                    Bracket::Close(BracketContent::Variable(0, 0, 0)),
                    Bracket::Close(BracketContent::Component(0, 0)),
                    Bracket::Close(BracketContent::Variable(0, 0, 0)),
                    Bracket::Close(BracketContent::Component(0, 0)),
                ],
                vec![
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a")),
                    Bracket::Close(BracketContent::Terminal("a")),
                    Bracket::Close(BracketContent::Component(1, 0)),
                    Bracket::Close(BracketContent::Variable(0, 0, 0)),
                    Bracket::Close(BracketContent::Component(0, 0)),
                    Bracket::Close(BracketContent::Variable(0, 0, 0)),
                    Bracket::Close(BracketContent::Component(0, 0)),
                    Bracket::Close(BracketContent::Variable(0, 0, 0)),
                    Bracket::Close(BracketContent::Component(0, 0)),
                ],
            ]
        )
    }

    #[test]
    fn kth2 () {
        let chart = example_automaton2().fill_chart();
        let state = chart.1;

        assert_eq!(chart.0.get(&state).expect("missing root entry").len(), 1);
        
        let mut it = chart.into_iter();

        for i in 1..10 {
            assert!(it.kth(state, i).is_none(), "failed at {}", i);
        }
    }

    #[test]
    fn elements2 () {
        let chart = example_automaton2().fill_chart();

        assert_eq!(
            chart.clone().into_iter().take(10).count(),
            1
        );

        let it = chart.into_iter();
        
        let some_words = it.collect::<Vec<_>>();
        let first_three = example_words2();

        assert_eq!(
            some_words,
            first_three
        );
    }

    fn example_automaton2 () -> CykAutomaton<BracketContent<String>, Reverse<LogDomain<f64>>> {
        use grammars::lcfrs::Lcfrs;
        use integeriser::{HashIntegeriser, Integeriser};

        let Lcfrs{ rules, init }: Lcfrs<String, String, Reverse<LogDomain<f64>>>
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

        let mut int = HashIntegeriser::new();
        for rule in rules {
            int.integerise(rule);
        }
        
        let word: Vec<String> = vec!["a", "c", "b", "b", "d"].into_iter().map(|s| s.to_owned()).collect();
        CykAutomatonPersistentStorage::from_grammar(int.values().into_iter(), &int, init).intersect(0..=8, &word)
    }

    fn example_words2 () -> Vec<Vec<Bracket<BracketContent<String>>>> {
        vec![
            vec![
                Bracket::Open(BracketContent::Component(0, 0)),
                Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    
                    Bracket::Open(BracketContent::Component(2, 0)),
                    Bracket::Open(BracketContent::Variable(2, 0, 0)),
                        Bracket::Open(BracketContent::Component(5, 0)),
                        Bracket::Open(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Component(5, 0)),
                    Bracket::Close(BracketContent::Variable(2, 0, 0)),
                    Bracket::Close(BracketContent::Component(2, 0)),

                Bracket::Close(BracketContent::Variable(0, 0, 0)),
                Bracket::Open(BracketContent::Variable(0, 1, 0)),

                Bracket::Open(BracketContent::Component(4, 0)),
                Bracket::Open(BracketContent::Variable(4, 0, 0)),
                Bracket::Open(BracketContent::Component(7, 0)),
                Bracket::Open(BracketContent::Terminal("c".to_owned())),
                Bracket::Close(BracketContent::Terminal("c".to_owned())),
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
                            Bracket::Open(BracketContent::Terminal("b".to_owned())),
                            Bracket::Close(BracketContent::Terminal("b".to_owned())),
                            Bracket::Close(BracketContent::Component(6, 0)),
                        Bracket::Close(BracketContent::Variable(2, 1, 0)),
                        Bracket::Close(BracketContent::Component(2, 1)),

                    Bracket::Close(BracketContent::Variable(1, 0, 1)),
                    Bracket::Open(BracketContent::Variable(1, 2, 0)),

                        Bracket::Open(BracketContent::Component(6, 0)),
                        Bracket::Open(BracketContent::Terminal("b".to_owned())),
                        Bracket::Close(BracketContent::Terminal("b".to_owned())),
                        Bracket::Close(BracketContent::Component(6, 0)),
                                 
                    Bracket::Close(BracketContent::Variable(1, 2, 0)),
                    Bracket::Close(BracketContent::Component(1, 1)),

                Bracket::Close(BracketContent::Variable(0, 0, 1)),
                Bracket::Open(BracketContent::Variable(0, 1, 1)),

                Bracket::Open(BracketContent::Component(4, 1)),
                Bracket::Open(BracketContent::Variable(4, 1, 0)),
                Bracket::Open(BracketContent::Component(8, 0)),
                Bracket::Open(BracketContent::Terminal("d".to_owned())),
                Bracket::Close(BracketContent::Terminal("d".to_owned())),
                Bracket::Close(BracketContent::Component(8, 0)),
                Bracket::Close(BracketContent::Variable(4, 1, 0)),
                Bracket::Close(BracketContent::Component(4, 1)),
                
                Bracket::Close(BracketContent::Variable(0, 1, 1)),
                Bracket::Close(BracketContent::Component(0, 0)),
            ]
        ]
    }
}