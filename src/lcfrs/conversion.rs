/// This module contains the implementation of the construction of an
/// LCFRS for each MCFG.

use PMCFGRule;
use mcfg::Mcfg;
use VarT;
use std::hash::Hash;
use std::collections::{BTreeSet, HashMap};
use super::*;

/// Implements the conversion from any `Mcfg` into a non-deleting `Mcfg`.
impl<N, T, W> From<Mcfg<N, T, W>> for Lcfrs<(N, BTreeSet<usize>), T, W>
where
    N: Hash + Eq + Ord + Clone,
    T: Clone,
    W: Copy
{
    fn from(mcfg: Mcfg<N, T, W>) -> Self {
        use recognisable::Search;

        let (mcfg_rules, mcfg_initial) = mcfg.destruct();

        let fanouts = read_fanouts(&mcfg_rules).unwrap();
        let mut rulemap: HashMap<&N, Vec<&PMCFGRule<N, T, W>>> = HashMap::new();
        for rule in &mcfg_rules {
            rulemap
                .entry(&rule.head)
                .or_insert_with(Vec::new)
                .push(rule);
        }

        let mut rules = Vec::new();

        for (a, deletions) in Search::unweighted(
            vec![(&mcfg_initial, BTreeSet::new())],
            |&(a, ref deltetions)| {
                let mut successors = Vec::new();
                for rule in rulemap.get(a).unwrap_or(&Vec::new()) {
                    let localfanouts_: Option<Vec<usize>> = rule.tail
                        .iter()
                        .map(|nt| fanouts.get(&nt).map(|f| *f))
                        .collect();
                    let rem_comp: Vec<_> = rule.composition
                        .composition
                        .iter()
                        .enumerate()
                        .filter_map(|(j, c)| {
                            if deltetions.contains(&j) {
                                None
                            } else {
                                Some(c)
                            }
                        })
                        .collect();
                    if let Some(localfanouts) = localfanouts_ {
                        successors.extend(
                            rule.tail
                                .iter()
                                .zip(composition_deletes(&rem_comp, &localfanouts)),
                        );
                    }
                }
                successors
            },
        ).uniques()
        {
            for rule in rulemap.get(a).unwrap_or(&Vec::new()) {
                rules.push(to_lcfrs_rule((*rule).clone(), deletions.clone(), &fanouts));
            }
        }

        Lcfrs {
            rules,
            init: (mcfg_initial, BTreeSet::new()),
        }
    }
}

/// For each successor, yields a set of components that are deleted in this composition.
fn composition_deletes<T>(
    composition: &Vec<&Vec<VarT<T>>>,
    fanouts: &[usize],
) -> Vec<BTreeSet<usize>> {
    let mut deletions: Vec<BTreeSet<usize>> = (0..fanouts.len())
        .map(|i| (0..fanouts[i]).collect())
        .collect();

    for (i, j) in composition
        .iter()
        .flat_map(|component| component.iter())
        .filter_map(|symbol| match *symbol {
            VarT::Var(i, j) => Some((i, j)),
            _ => None,
        }) {
        deletions[i].remove(&j);
    }

    deletions
}

/// Performs the conversion of an `Mcfg` rule to an lcfrs rule.
fn to_lcfrs_rule<N, T, W>(
    rule: PMCFGRule<N, T, W>,
    deletions: BTreeSet<usize>,
    fanouts: &HashMap<&N, usize>,
) -> PMCFGRule<(N, BTreeSet<usize>), T, W> 
where N: Hash + Eq
{
    use Composition;
    
    let PMCFGRule {
        head,
        tail,
        weight,
        composition,
    } = rule;

    let rem_comp: Vec<_> = composition
        .composition
        .into_iter()
        .enumerate()
        .filter_map(|(j, c)| {
            if deletions.contains(&j) {
                None
            } else {
                Some(c)
            }
        })
        .collect();
    let succ_fanouts: Vec<usize> = tail.iter().map(|n| *fanouts.get(&n).unwrap()).collect();
    let successors_deletions = composition_deletes(
        &rem_comp.iter().collect(),
        &succ_fanouts,
    );

    let processed_composition: Vec<Vec<VarT<T>>> = rem_comp.into_iter()
        .map(|component| {
            component
                .into_iter()
                .map(|symbol| match symbol {
                    VarT::Var(i, j) => VarT::Var(
                        i,
                        j - successors_deletions[i].iter().filter(|j_| **j_ < j).count(),
                    ),
                    t => t,
                })
                .collect()
        })
        .collect();

    PMCFGRule {
        head: (head, deletions),
        composition: Composition{ composition: processed_composition },
        tail: tail.into_iter().zip(successors_deletions.into_iter()).collect(),
        weight,
    }
}