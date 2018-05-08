/// This module implements the better k-best parsing algorithm
/// described by Huang and Chiang in 2005 in Algorithm 3.

use std::collections::{BinaryHeap, HashMap};
use super::*;
use std::iter::repeat;
use std::ops::Mul;
use dyck::Bracket;
use util::{reverse::Reverse, tree::GornTree};

pub struct ChartIterator<T, W>
where
    T: Hash + Eq,
{
    chart: GenerationChart<W>,
    fsa: ExplodedAutomaton<T, W>,

    d: HashMap<(u32, u32), Vec<(ChartEntry<W>, Vec<usize>, W)>>,
    c: HashMap<(u32, u32), BinaryHeap<(Reverse<W>, ChartEntry<W>, Reverse<Vec<usize>>)>>,
    
    k: usize
}

impl<T, W> ChartIterator<T, W>
where
    T: Eq + Hash,
    W: Mul<Output=W> + Ord + Copy
{
    pub fn new(chart: GenerationChart<W>, fsa: ExplodedAutomaton<T, W>) -> Self {
        ChartIterator {
            chart,
            fsa,
            d: HashMap::new(),
            c: HashMap::new(),
            k: 0
        }
    }
    
    fn kth(&mut self, state: (u32, u32), k: usize) -> Option<(ChartEntry<W>, Vec<usize>, W)> {
        use self::ChartEntry::*;

        if let Some(deriv) = self.d.get(&state).and_then(|v| v.get(k)) {
            return Some(deriv.clone())
        }

        if self.c.get(&state).is_none() {
            self.c.insert(
                state, 
                self.chart.0.get(&state).into_iter().flat_map(|v| v).map(
                    |&(ce, w)| (w.into(), ce, repeat(0).take(ce.dim()).collect::<Vec<_>>().into())
                ).collect()
            );
            self.d.insert(
                state,
                self.c.get_mut(&state).and_then(|h| h.pop()).map(|(w, ce, v)| (ce, v.unwrap(), w.unwrap())).into_iter().collect()
            );
        }

        while self.d.get(&state).unwrap().len() < (k + 1) {
            if let Some((ce, i, _)) = self.d.get(&state).unwrap().last().map(|t| t.clone()) {

                for d in 0 .. ce.dim() {
                    let mut i_ = i.clone();
                    i_[d] += 1;
                    
                    match ce {
                        Concat(p, q, q2) => {
                            if let (Some(w1), Some(w2)) = (self.kth((p, q), i_[0]).map(|t| t.2), self.kth((q, q2), i_[1]).map(|t| t.2)) {
                                if !self.c.get(&state).unwrap().iter().any(|&(_, ref ce_, ref i__)| &ce == ce_ && &i_ == i__.inner()) {
                                    self.c.get_mut(&state).unwrap().push(((w1 * w2).into(), ce, i_.into()));
                                }
                            }
                        },
                        Bracketed(_, w1, p, q, w2) => {
                            if let Some(wi) = self.kth((p, q), i_[0]).map(|t| t.2) {
                                if !self.c.get(&state).unwrap().iter().any(|&(_, ref ce_, ref i__)| &ce == ce_ && &i_ == i__.inner()) {
                                    self.c.get_mut(&state).unwrap().push(((w1 * wi * w2).into(), ce, i_.into()));
                                }
                            }
                        }
                        _ => ()
                    }
                }

            }

            if self.c.get(&state).unwrap().is_empty() {
                break;
            }
            
            self.d.get_mut(&state).unwrap().push(
                self.c.get_mut(&state).unwrap().pop().map(|(w, ce, i)| (ce, i.unwrap(), w.unwrap())).unwrap()
            )

        }

        self.d.get(&state).and_then(|v| v.get(k)).map(|t| (*t).clone())
    }

    fn read(&mut self, ce: &ChartEntry<W>, i: &[usize]) -> Vec<Bracket<T>>
    where
        T: Clone
    {
        use self::ChartEntry::*;

        match *ce {
            Concat(p, q, q2) => {
                let (ce1, i1, _) = self.kth((p, q), i[0]).unwrap();
                let (ce2, i2, _) = self.kth((q, q2), i[1]).unwrap();
                let mut w = self.read(&ce1, &i1);
                w.extend(self.read(&ce2, &i2));
                w
            },
            Bracketed(ti, _, p, q, _) => {
                let (ce_, i_, _) = self.kth((p, q), i[0]).unwrap();
                let mut w = self.read(&ce_, &i_);
                let t = self.fsa.bracket_i.find_value(ti as usize).unwrap();
                
                w.insert(0, Bracket::Open(t.clone()));
                w.push(Bracket::Close(t.clone()));
                w
            }
            Initial(wi, _) => {
                self.fsa.terminal_i.find_value(wi as usize).unwrap().clone()
            }
        }
    }
}

impl<T, W> Iterator for ChartIterator<T, W>
where
    T: Eq + Hash + Clone,
    W: Ord + Mul<Output=W> + Copy + ::std::fmt::Debug
{
    type Item = Vec<Bracket<T>>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.fsa.finals.is_empty() {
            None
        } else {
            let state = (self.fsa.initial, self.fsa.finals[0]);
            let k = self.k;
            self.k += 1;

            self.kth(state, k).map(
                |(ce, i, _)| self.read(&ce, &i)
            )
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use super::super::*;
    use super::super::tests::example_fsa;
    use num_traits::One;
    use log_domain::LogDomain;
    use grammars::lcfrs::cs_representation::automata::Delta;
    
    #[test]
    fn kth() {
        let one = LogDomain::one().into();
        let w1 = LogDomain::new(0.5).unwrap().into();
        let w2 = LogDomain::new(0.75).unwrap().into();
        
        use self::ChartEntry::*;

        let exploded = ExplodedAutomaton::new(example_fsa());
        let chart = GenerationChart::fill(&exploded, Capacity::Infinite);

        let t1 = exploded.bracket_i.find_key(&BracketContent::Variable(0, 0, 0)).unwrap();
        let t2 = exploded.bracket_i.find_key(&BracketContent::Component(0, 0)).unwrap();
        
        let mut it = ChartIterator::new(chart, exploded);

        assert_eq!(
            it.kth((0, 1), 0),
            Some((Initial(0, w2), Vec::new(), w2))
        );

        assert_eq!(
            it.kth((2, 3), 0),
            Some((Bracketed(t1 as u32, one, 0, 1, one), vec![0], w2))
        );

        assert_eq!(
            it.kth((0, 1), 1),
            Some((Bracketed(t2 as u32, w1, 2, 3, w1), vec![0], w1 * w2 * w1))
        );

    }

    #[test]
    fn structure() {
        let one: Reverse<LogDomain<f64>> = LogDomain::one().into();
        let w1: Reverse<LogDomain<f64>> = LogDomain::new(0.5).unwrap().into();
        let w2 = LogDomain::new(0.75).unwrap().into();

        let exploded = ExplodedAutomaton::new(example_fsa());
        let chart = GenerationChart::fill(&exploded, Capacity::Infinite);

        let t1 = exploded.bracket_i.find_key(&BracketContent::Component(0, 0)).unwrap() as u32;
        let t2 = exploded.bracket_i.find_key(&BracketContent::Variable(0, 0, 0)).unwrap() as u32;

        let mut it = ChartIterator::new(chart, exploded);

        assert!(it.d.is_empty());
        assert!(it.c.is_empty());
        assert_eq!(it.k, 0);

        assert!(it.next().is_some());

        assert_eq!(
            it.d,
            vec![
                ( (0, 1), 
                  vec![
                    (ChartEntry::Initial(0, w2), Vec::new(), w2)
                  ]
                )
            ].into_iter().collect()
        );
    
        let w_: Reverse<LogDomain<f64>> = w1 * w2 * w1;
        let w: Reverse<Reverse<LogDomain<f64>>> = w_.into();
        assert_eq!(
            it.c.iter().map(|(k, h)| (k.clone(), h.clone().into_sorted_vec())).collect::<Vec<_>>(),
            vec![
                ( (0, 1), 
                  vec![
                    (w, ChartEntry::Bracketed(t1, w1, 2, 3, w1), vec![0].into())
                  ]
                )
            ]
        );

        assert!(it.next().is_some());
        assert_eq!(
            it.d,
            vec![
                ( (0, 1), 
                  vec![
                    (ChartEntry::Initial(0, w2), Vec::new(), w2),
                    (ChartEntry::Bracketed(t1, w1, 2, 3, w1), vec![0], w1 * w2 * w1),
                  ]
                ),
                ( (2, 3), 
                  vec![
                    (ChartEntry::Bracketed(t2, one, 0, 1, one), vec![0], w2),
                  ]
                )
            ].into_iter().collect()
        );
    }

    #[test]
    fn elements() {
        let exploded = ExplodedAutomaton::new(example_fsa());
        let chart = GenerationChart::fill(&exploded, Capacity::Infinite);

        assert_eq!(
            ChartIterator::new(chart.clone(), exploded.clone()).take(10).count(),
            10
        );

        assert_eq!(
            ChartIterator::new(chart, exploded).take(4).collect::<Vec<_>>(),
            vec![
                vec![
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Component(1, 0)),
                ],
                vec![
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Terminal("a".to_owned())),
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
                    Bracket::Open(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Terminal("a".to_owned())),
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
                    Bracket::Open(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Terminal("a".to_owned())),
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
        let exploded = ExplodedAutomaton::new(example_fsa2());
        let state = (exploded.initial, exploded.finals[0]);

        let chart = GenerationChart::fill(&exploded, Capacity::Infinite);

        assert_eq!(chart.0.get(&state).expect("missing root entry").len(), 1);
        
        let mut it = ChartIterator::new(chart, exploded);

        for i in 0..10 {
            assert!(it.kth(state, i).is_some(), "failed at {}", i);
        }
    }

    #[test]
    fn elements2 () {
        use std::collections::HashSet;

        let exploded = ExplodedAutomaton::new(example_fsa2());
        let chart = GenerationChart::fill(&exploded, Capacity::Infinite);

        assert_eq!(
            ChartIterator::new(chart.clone(), exploded.clone()).take(10).count(),
            10
        );

        let it = ChartIterator::new(chart, exploded);
        
        let some_words = it.take(3).collect::<Vec<_>>();
        let first_three = example_words2();

        assert_eq!(
            some_words[0],
            first_three[0]
        );

        assert_eq!(
            some_words[1..3].iter().cloned().collect::<HashSet<_>>(),
            first_three[1..3].iter().cloned().collect::<HashSet<_>>()
        );
    }

    fn example_fsa2 () -> FiniteAutomaton<BracketFragment<String>, Reverse<LogDomain<f64>>> {
        use grammars::lcfrs::Lcfrs;
        use integeriser::{HashIntegeriser, Integeriser};
        use super::super::super::Generator;

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
        
        Generator::unboxed_push_down(int.clone().values(), init, &int).approximate(0)
    }

    fn example_words2 () -> Vec<Vec<Delta<String>>> {
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

                Bracket::Open(BracketContent::Component(2, 1)),
                Bracket::Open(BracketContent::Variable(2, 1, 0)),
                Bracket::Open(BracketContent::Component(6, 0)),
                Bracket::Open(BracketContent::Terminal("b".to_owned())),
                Bracket::Close(BracketContent::Terminal("b".to_owned())),
                Bracket::Close(BracketContent::Component(6, 0)),
                Bracket::Close(BracketContent::Variable(2, 1, 0)),
                Bracket::Close(BracketContent::Component(2, 1)),

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
            ],
            vec![
                Bracket::Open(BracketContent::Component(0, 0)),
                Bracket::Open(BracketContent::Variable(0, 0, 0)),

                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Variable(1, 0, 0)),
                    
                        Bracket::Open(BracketContent::Component(2, 0)),
                        Bracket::Open(BracketContent::Variable(2, 0, 0)),
                            Bracket::Open(BracketContent::Component(5, 0)),
                            Bracket::Open(BracketContent::Terminal("a".to_owned())),
                            Bracket::Close(BracketContent::Terminal("a".to_owned())),
                            Bracket::Close(BracketContent::Component(5, 0)),
                        Bracket::Close(BracketContent::Variable(2, 0, 0)),
                        Bracket::Close(BracketContent::Component(2, 0)),

                    Bracket::Close(BracketContent::Variable(1, 0, 0)),
                    Bracket::Open(BracketContent::Variable(1, 1, 0)),
                        
                        Bracket::Open(BracketContent::Component(5, 0)),
                        Bracket::Open(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Component(5, 0)),
                    
                    Bracket::Close(BracketContent::Variable(1, 1, 0)),
                    Bracket::Close(BracketContent::Component(1, 0)),

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

                Bracket::Open(BracketContent::Component(2, 1)),
                Bracket::Open(BracketContent::Variable(2, 1, 0)),
                Bracket::Open(BracketContent::Component(6, 0)),
                Bracket::Open(BracketContent::Terminal("b".to_owned())),
                Bracket::Close(BracketContent::Terminal("b".to_owned())),
                Bracket::Close(BracketContent::Component(6, 0)),
                Bracket::Close(BracketContent::Variable(2, 1, 0)),
                Bracket::Close(BracketContent::Component(2, 1)),

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
            ],
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