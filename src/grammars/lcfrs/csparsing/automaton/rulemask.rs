use super::{RuleIdT, StateT};

use crate::grammars::pmcfg::PMCFGRule;

use fnv::FnvHashMap;
use integeriser::{HashIntegeriser, Integeriser};
use std::cmp::max;
use std::{collections::HashSet, hash::Hash};

use vecmultimap::VecMultiMap;

/// Serializable filter for grammar rules.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleMaskBuilder<T>
where
    T: Eq + Hash,
{
    initial_nt: StateT,
    terminal: FnvHashMap<T, Vec<StateT>>,
    inner: Vec<Vec<(Vec<StateT>, StateT)>>, // Save some storage using binary rules
    td_inner: Vec<Vec<(Vec<RuleIdT>, Vec<StateT>)>>,
    ruleids: usize,
    states: usize,
}

impl<T> RuleMaskBuilder<T>
where
    T: Clone + Eq + Hash,
{
    pub fn new<'a, N, W>(
        rules: impl Iterator<Item = &'a PMCFGRule<N, T, W>> + 'a,
        initial: &N,
    ) -> Self
    where
        N: Eq + Hash + 'a,
        T: 'a,
        W: 'a,
    {
        let mut rid_counter: StateT = 0;

        let mut integeriser: HashIntegeriser<&N> = HashIntegeriser::new();
        let mut terminal: FnvHashMap<T, Vec<StateT>> = FnvHashMap::default();
        let mut inner = VecMultiMap::new();
        let mut inner_td = VecMultiMap::new();

        for rule in rules {
            let nid = integeriser.integerise(&rule.head) as StateT;

            if rule.composition.len() == 1
                && rule.composition.composition[0].len() == 1
                && rule.composition.composition[0][0].is_t()
            {
                terminal
                    .entry(rule.composition.composition[0][0].unwrap_t().clone())
                    .or_default()
                    .push(nid);
                inner_td.push_to(nid as usize, (vec![rid_counter], vec![]));
                rid_counter += 1;
            } else {
                let rids = rule
                    .composition
                    .iter()
                    .flat_map(|component| {
                        let new_rids = max(component.len() - 1, 1) as RuleIdT;
                        rid_counter += new_rids;
                        ((rid_counter - new_rids)..rid_counter)
                    })
                    .collect::<Vec<_>>();

                let i_rhs = rule
                    .tail
                    .iter()
                    .map(|i| integeriser.integerise(i) as StateT)
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect::<Vec<_>>();
                for rhs_index in 0..(i_rhs.len()) {
                    let mut rhs_remainder = i_rhs.clone();
                    inner.push_to(
                        rhs_remainder.remove(rhs_index) as usize,
                        (rhs_remainder, nid),
                    );
                }
                inner_td.push_to(nid as usize, (rids, i_rhs));
            }
        }

        let initial_nt = integeriser.find_key(&initial).unwrap() as StateT;

        Self {
            initial_nt,
            terminal,
            inner: inner.into_vec_with_size(integeriser.size()),
            td_inner: inner_td.into_vec_with_size(integeriser.size()),
            ruleids: rid_counter as usize,
            states: integeriser.size(),
        }
    }

    pub fn build(&self, word: &[T]) -> Vec<bool> {
        use std::mem::replace;

        let mut runtime_stack = Vec::with_capacity(self.states);
        let mut productive_nts = vec![false; self.states];
        let mut reachable_nts = vec![false; self.states];
        let mut productive_and_reachable_rules = vec![false; self.ruleids];

        // ## step 1: search for productive nonterminals and rules

        for t in word {
            if let Some(v) = self.terminal.get(t) {
                runtime_stack.extend(v.iter().cloned());
            }
        }

        while let Some(nt) = runtime_stack.pop() {
            if replace(&mut productive_nts[nt as usize], true) {
                continue;
            }
            runtime_stack.extend(self.inner[nt as usize].iter().filter_map(
                |&(ref other_nts, top)| {
                    if !productive_nts[top as usize]
                        && other_nts.iter().all(|nt| productive_nts[*nt as usize])
                    {
                        Some(top)
                    } else {
                        None
                    }
                },
            ));
        }

        // ## step 2: search for reachable nonterminals and rules among the productive ones

        runtime_stack.push(self.initial_nt);
        while let Some(nt) = runtime_stack.pop() {
            if replace(&mut reachable_nts[nt as usize], true) || !productive_nts[nt as usize] {
                continue;
            }
            for &(ref ruleids, ref bottom_states) in &self.td_inner[nt as usize] {
                if bottom_states.iter().all(|nt| productive_nts[*nt as usize]) {
                    for rule_id in ruleids {
                        productive_and_reachable_rules[*rule_id as usize] = true;
                    }
                    runtime_stack.extend(bottom_states);
                }
            }
        }

        productive_and_reachable_rules
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::grammars::pmcfg::{Composition, VarT};

    fn hots<I: IntoIterator<Item = usize>>(is: I, len: usize) -> Vec<bool> {
        let mut v = vec![false; len];
        for i in is {
            v[i] = true;
        }
        v
    }

    #[test]
    fn filter() {
        let rules = vec![
            PMCFGRule {
                head: "A",
                tail: vec!["A", "T1", "T2"],
                weight: 1f64,
                composition: Composition::from(vec![
                    vec![VarT::Var(1, 0), VarT::Var(0, 0)],
                    vec![VarT::Var(0, 1), VarT::Var(2, 0)],
                ]),
            },
            PMCFGRule {
                head: "A",
                tail: vec!["B"],
                weight: 1f64,
                composition: Composition::from(vec![
                    vec![VarT::Var(1, 0), VarT::Var(0, 0)],
                    vec![VarT::Var(0, 1), VarT::Var(2, 0)],
                ]),
            },
            PMCFGRule {
                head: "A",
                tail: vec!["T0", "T0"],
                weight: 1f64,
                composition: Composition::from(vec![vec![VarT::Var(0, 0)], vec![VarT::Var(1, 0)]]),
            },
            PMCFGRule {
                head: "B",
                tail: vec!["T3", "T3"],
                weight: 1f64,
                composition: Composition::from(vec![vec![VarT::Var(0, 0)], vec![VarT::Var(1, 0)]]),
            },
            PMCFGRule {
                head: "C",
                tail: vec!["T3", "T3"],
                weight: 1f64,
                composition: Composition::from(vec![vec![VarT::Var(0, 0)], vec![VarT::Var(1, 0)]]),
            },
            PMCFGRule {
                head: "T0",
                tail: vec![],
                weight: 1f64,
                composition: Composition::from(vec![vec![VarT::T(0)]]),
            },
            PMCFGRule {
                head: "T1",
                tail: vec![],
                weight: 1f64,
                composition: Composition::from(vec![vec![VarT::T(1)]]),
            },
            PMCFGRule {
                head: "T2",
                tail: vec![],
                weight: 1f64,
                composition: Composition::from(vec![vec![VarT::T(2)]]),
            },
            PMCFGRule {
                head: "T3",
                tail: vec![],
                weight: 1f64,
                composition: Composition::from(vec![vec![VarT::T(3)]]),
            },
        ];

        let filter = RuleMaskBuilder::new(rules.iter(), &"A");

        assert_eq!(filter.build(&[]), vec![false; 14]);
        assert_eq!(filter.build(&[1]), vec![false; 14]);
        assert_eq!(filter.build(&[2]), vec![false; 14]);
        assert_eq!(filter.build(&[1, 2]), vec![false; 14]);
        assert_eq!(filter.build(&[0, 1]), hots(vec![4, 5, 10], 14));
        assert_eq!(
            filter.build(&[0, 1, 2]),
            hots(vec![0, 1, 4, 5, 10, 11, 12], 14)
        );
        assert_eq!(
            filter.build(&[1, 2, 3]),
            hots(vec![0, 1, 2, 3, 6, 7, 11, 12, 13], 14)
        );
        assert_eq!(
            filter.build(&[0, 1, 2, 3]),
            hots(vec![0, 1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13], 14)
        );
    }
}
