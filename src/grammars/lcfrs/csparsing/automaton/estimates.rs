use super::{Automaton, RangeT, StateT};
use num_traits::{One, Zero};
use std::mem::replace;
use std::{collections::BinaryHeap, hash::Hash, ops::Mul};

/// Sx inside estimation for the cfg. approximation.
// Values up to spans of length `n` and for `q` states are stored in a
// `n ⋅ q` vector in state-first layout.
// The number of states is stored along with the vector.
#[derive(Debug)]
struct SxInside<W>(Vec<W>, usize);

/// Sx outside estimation for the cfg. approximation.
// Values up to spans of length `n` and for `q` states are stored in a
// `n ⋅ (n+1) ⋅ q / 2` vector (=CYK chart) in state-first layout.
// The maximum span width and the number of states is stored along with the vector.
#[derive(Debug, Serialize, Deserialize)]
pub struct SxOutside<W>(Vec<W>, usize, usize);

impl<W: Zero + Copy + Ord + Mul<Output = W>> SxInside<W> {
    fn index(&self, q: StateT, range: RangeT) -> usize {
        self.1 * (range - 1) as usize + q as usize
    }

    fn with_capacity(states: usize, maxrange: usize) -> Self {
        Self(vec![W::zero(); states * maxrange], states)
    }

    fn insert(&mut self, q: StateT, range: RangeT, weight: W) -> bool {
        let index = self.index(q, range);
        let w = &mut self.0[index];
        if *w == W::zero() {
            *w = weight;
            true
        } else {
            false
        }
    }

    fn get(&self, q: StateT, range: RangeT) -> Option<W> {
        let w = self.0[self.index(q, range)];
        if w == W::zero() {
            None
        } else {
            Some(w)
        }
    }

    fn iterate_states<'a>(&'a self, range_size: RangeT) -> impl 'a + Iterator<Item = (StateT, W)> {
        let start = (range_size as usize - 1) * self.1;
        self.0[start..(start + self.1)]
            .iter()
            .enumerate()
            .filter_map(|(q, w)| {
                if *w != W::zero() {
                    Some((q as StateT, *w))
                } else {
                    None
                }
            })
    }

    /// Constructs a structure storing the Sx inside estimate for the given cfg.
    /// The estimates are computed up to a given span.
    fn from_automaton<T: Hash + Eq>(automaton: &Automaton<T, W>, maxrange: RangeT) -> Self {
        let mut insides = Self::with_capacity(automaton.0.len(), maxrange as usize);
        let mut queue: BinaryHeap<(W, StateT)> = BinaryHeap::new();

        for range_size in 1..=maxrange {
            if range_size == 1 {
                queue.extend(
                    automaton
                        .2
                        .values()
                        .flat_map(|v| v.iter().map(|&(_, (w, q))| (w, q))),
                );
            }

            for left_portion in 1..range_size {
                let right_portion = range_size - left_portion;
                for (ql, wl) in insides.iterate_states(left_portion) {
                    queue.extend(automaton.0[ql as usize].iter().filter_map(
                        |&(_, qr, (w0, q0))| {
                            insides
                                .get(qr, right_portion)
                                .map(move |wr| (wl * w0 * wr, q0))
                        },
                    ));
                }
            }

            let mut filter = vec![false; insides.1];
            while let Some((w1, q1)) = queue.pop() {
                if replace(&mut filter[q1 as usize], true) {
                    continue;
                }
                insides.insert(q1, range_size, w1);
                queue.extend(
                    automaton.1[q1 as usize]
                        .iter()
                        .filter_map(|&(_, (w0, q0))| {
                            if !filter[q0 as usize] {
                                Some((w0, q0))
                            } else {
                                None
                            }
                        }),
                );
            }
        }
        insides
    }
}

impl<W: Copy + Ord + Mul<Output = W> + Zero> SxOutside<W> {
    fn index(&self, q: StateT, i: RangeT, j: RangeT) -> usize {
        ((self.1 * (self.1 + 1)
            - (self.1 - (j - i) as usize + 1) * (self.1 - (j - i) as usize + 2))
            / 2
            + i as usize)
            * self.2
            + q as usize
    }
    fn index_usize(&self, q: StateT, i: usize, j: usize) -> usize {
        ((self.1 * (self.1 + 1) - (self.1 - (j - i) + 1) * (self.1 - (j - i) + 2)) / 2 + i) * self.2
            + q as usize
    }

    fn with_capacity(states: usize, maxrange: usize) -> Self {
        Self(
            vec![W::zero(); states * maxrange * (maxrange + 1) / 2],
            maxrange,
            states,
        )
    }

    fn insert(&mut self, q: StateT, i: RangeT, j: RangeT, weight: W) -> bool {
        let index = self.index(q, i, j);
        let w = &mut self.0[index];
        if *w == W::zero() {
            *w = weight;
            true
        } else {
            false
        }
    }

    fn iterate_states<'a>(
        &'a self,
        left_front: RangeT,
        right_front: RangeT,
    ) -> impl 'a + Iterator<Item = (StateT, W)> {
        let start = self.index(0, left_front, right_front);
        let end = start + self.2;
        self.0[start..end].iter().enumerate().filter_map(|(q, w)| {
            if *w != W::zero() {
                Some((q as StateT, *w))
            } else {
                None
            }
        })
    }

    /// Fetch the Sx outside estimation for the constituent `q` spanning `(i, j)`.
    /// This method will return
    /// * `None`, if there is no outside estimate,
    /// * `Some(W::one())`, if the given spans are out of the maximum range, or
    /// * `Some(w)`, where w is the outside estimate.
    pub fn get(&self, q: StateT, i: usize, j: usize, n: usize) -> Option<W>
    where
        W: One,
    {
        if i + n - j >= self.1 {
            return Some(W::one());
        }
        let weight = self.0[self.index_usize(q, i, j + self.1 - n)];
        if weight == W::zero() {
            None
        } else {
            Some(weight)
        }
    }

    /// Constructs a structure storing the Sx outside estimate for the given cfg.
    /// The estimates are computed up to a given span.
    pub fn from_automaton<T: Hash + Eq>(automaton: &Automaton<T, W>, maxrange: RangeT) -> Self
    where
        W: One,
    {
        let insides = SxInside::from_automaton(automaton, maxrange);
        let mut outsides = Self::with_capacity(automaton.0.len(), maxrange as usize);
        let mut queue: BinaryHeap<(W, StateT)> = BinaryHeap::new();

        for range_size in (1..=maxrange).rev() {
            for left_front in 0..=(maxrange - range_size) {
                let right_front = left_front + range_size;

                if range_size == maxrange {
                    queue.push((W::one(), automaton.7));
                }

                for extended_left_front in 0..left_front {
                    for (q0, wo) in outsides.iterate_states(extended_left_front, right_front) {
                        queue.extend(automaton.3[q0 as usize].iter().filter_map(
                            |&(_, ql, qr, w0)| {
                                let wl = insides.get(ql, left_front - extended_left_front)?;
                                Some((wl * w0 * wo, qr))
                            },
                        ))
                    }
                }

                for extended_right_front in (right_front + 1)..=maxrange {
                    for (q0, wo) in outsides.iterate_states(left_front, extended_right_front) {
                        queue.extend(automaton.3[q0 as usize].iter().filter_map(
                            |&(_, ql, qr, w0)| {
                                let wr = insides.get(qr, extended_right_front - right_front)?;
                                Some((w0 * wr * wo, ql))
                            },
                        ))
                    }
                }

                let mut filter = vec![false; outsides.2];
                while let Some((w0, q0)) = queue.pop() {
                    if replace(&mut filter[q0 as usize], true) {
                        continue;
                    }
                    outsides.insert(q0, left_front, right_front, w0);
                    queue.extend(automaton.4[q0 as usize].iter().filter_map(|&(_, q1, w1)| {
                        if !filter[q1 as usize] {
                            Some((w1 * w0, q1))
                        } else {
                            None
                        }
                    }));
                }
            }
        }
        outsides
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::grammars::{
        lcfrs::Lcfrs,
        pmcfg::{Composition, PMCFGRule, VarT},
    };
    use log_domain::LogDomain;

    #[test]
    fn estimates() {
        let gmr = lcfrs();
        let one = LogDomain::one();
        let w1 = LogDomain::new(0.3f64).unwrap();
        let w2 = LogDomain::new(0.7f64).unwrap();

        let g = Automaton::from_grammar(
            gmr.rules.iter().enumerate().map(|(i, r)| (i as u32, r)),
            gmr.init,
        );
        let inside = SxInside::from_automaton(&g, 3);
        assert_eq!(inside.get(0, 1), Some(w2));
        assert_eq!(inside.get(0, 2), Some(w1 * w2 * w2));
        assert_eq!(inside.get(0, 3), Some(w1 * w1 * w2 * w2 * w2));

        let outside = SxOutside::from_automaton(&g, 3);
        assert_eq!(outside.get(0, 0, 3, 3), Some(one));
        assert_eq!(outside.get(0, 1, 2, 3), Some(w1 * w2 * w1 * w2));
        assert_eq!(outside.get(0, 1, 2, 4), Some(one));
    }

    fn lcfrs() -> Lcfrs<&'static str, char, LogDomain<f64>> {
        Lcfrs {
            init: "S",
            rules: vec![
                PMCFGRule {
                    head: "S",
                    tail: vec!["S", "S"],
                    composition: Composition {
                        composition: vec![vec![VarT::Var(0, 0), VarT::Var(1, 0)]],
                    },
                    weight: LogDomain::new(0.3f64).unwrap(),
                },
                PMCFGRule {
                    head: "S",
                    tail: vec![],
                    composition: Composition {
                        composition: vec![vec![VarT::T('A')]],
                    },
                    weight: LogDomain::new(0.7f64).unwrap(),
                },
            ],
        }
    }
}
