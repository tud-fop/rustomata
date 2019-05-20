use super::*;
use crate::dyck::Bracket;
use crate::grammars::pmcfg::{Composition, PMCFGRule, VarT};
use crate::util::{tree::GornTree, IntMap};
use fnv::{FnvHashMap, FnvHashSet};
use std::collections::{BTreeMap, BTreeSet};
use vecmultimap::VecMultiMapAdapter;

/// The node in a `FailedParseTree`.
#[derive(PartialEq, Debug)]
pub struct FailedParseTreeNode {
    // the applied grammar rule
    rule: u32,
    // the components this grammar rule was used in
    components: FnvHashSet<u8>,

    // successor -> rule -> tree
    children: BTreeMap<u8, FnvHashMap<u32, usize>>,
}

/// Represents a failed attempt to parse a word using the Chomsky-Schützenberger parser.
/// Each node has a contains a list of successors for each successor nt of the contained grammar rule.
pub struct FailedParseTree(IntMap<FailedParseTreeNode>);

impl FailedParseTree {
    /// Reads off a `FailedParseTree` from a Dyck word over `Delta`.
    pub fn new(word: &[Delta]) -> Self {
        let mut tree = IntMap::default();
        let mut unique = 0;
        let mut pos = 0;

        let mut position_stack = Vec::new();

        match word.first() {
            Some(&Bracket::Open(BracketContent::Component(rule, 0))) => {
                tree.insert(
                    0,
                    FailedParseTreeNode {
                        rule,
                        components: vec![0].into_iter().collect(),
                        children: BTreeMap::new(),
                    },
                );
            }
            _ => panic!(),
        }

        position_stack.push(0);

        for i in 1..word.len() {
            match word[i] {
                Bracket::Open(BracketContent::Variable(_, succ, comp)) => match word[i + 1] {
                    Bracket::Open(BracketContent::Component(rule, _)) => {
                        let t: usize = *tree
                            .get_mut(&pos)
                            .unwrap()
                            .children
                            .entry(succ)
                            .or_insert_with(FnvHashMap::default)
                            .entry(rule)
                            .or_insert_with(|| {
                                unique += 1;
                                unique
                            });
                        tree.entry(t)
                            .or_insert(FailedParseTreeNode {
                                rule,
                                components: FnvHashSet::default(),
                                children: BTreeMap::new(),
                            })
                            .components
                            .insert(comp);
                        position_stack.push(pos);
                        pos = t;
                    }
                    _ => panic!(),
                },
                Bracket::Close(BracketContent::Variable(_, _, _)) => {
                    pos = position_stack.pop().unwrap();
                }
                _ => (),
            }
        }

        FailedParseTree(tree)
    }

    /// Counts the occurences of unambiguous grammar applications.
    #[allow(dead_code)]
    pub fn fails(&self) -> usize {
        self.0
            .values()
            .flat_map(|node| node.children.values().map(|rs| rs.len()))
            .filter(|numer_of_applied_rules| *numer_of_applied_rules > 1)
            .count()
    }

    /// Merges a `FailedParseTree` into a derivation tree of a ``similar grammar''.
    /// If there is an umambiguous application of a grammar rule in the tree, the applied rules are merged
    /// into a single new grammar rule in the following fashion:
    /// * the components of the compositions of the applied grammar rules are merged into a new composition
    ///   (note that there is at most one occurence of each component index),
    /// * the variables in the merged components are reordered and increased s.t. the components merged from
    ///   the same rules reference the same success, but variables in components from different rules reference
    ///   different successors, and
    /// * we filter the list of successor nonterminals for each rule s.t. they only contain those that are
    ///   referenced by some variable in the merged components.
    pub fn merge<N, T, W>(&self, rules: &[PMCFGRule<N, T, W>]) -> GornTree<PMCFGRule<N, T, W>>
    where
        N: Clone,
        T: Clone,
        W: Copy + Zero,
    {
        let mut tree = GornTree::new();

        // failed tree positions + parse tree position
        let mut execution_stack = vec![(vec![0], Vec::new())];

        while let Some((ft_poss, pt_pos)) = execution_stack.pop() {
            // TODO Find an easy way to skip processing if only a single rule is applied.
            //      The current solution merges the FPT into some LCFRS rules;
            //      although, we can skip that if we allow MCFG derivation trees.
            //      The implementation of the export format in pmcfg/negra cannot handle
            //      empty compositions, even if no variable points to such an empty rule.
            // IDEA remove the successor and decrement variables for all following successors
            //
            // if ft_poss.len() == 1 {
            //     let &FailedParseTreeNode{ rule: rule_id, ref children, .. } = self.0.get(&ft_poss[0]).unwrap();
            //     let rule = integeriser.find_value(rule_id).unwrap().clone();

            //     for succ in 0..(rule.tail.len()) {
            //         let mut child_tree_node_position = pt_pos.clone();
            //         child_tree_node_position.push(succ);

            //         let successors = children.get(&succ);
            //         if successors.map_or(0, |m| m.len()) > 0 {
            //             execution_stack.push((successors.unwrap().values().cloned().collect(), child_tree_node_position));
            //         } else {
            //             tree.insert( // push empty rule
            //                 child_tree_node_position,
            //                 PMCFGRule{
            //                     head: rule.tail[succ].clone(),
            //                     tail: Vec::new(),
            //                     composition: Composition{ composition: Vec::new() },
            //                     weight: W::one()
            //                 }
            //             );
            //         }
            //     }

            //     tree.insert(pt_pos, rule);
            // } else {
            let (rule, child_tree_nodess) = merge_rules(ft_poss.into_iter().map(|i| {
                self.0
                    .get(&i)
                    .map(|tn| (&tn.components, &rules[tn.rule as usize], &tn.children))
                    .unwrap()
            }));

            for (s_pos, child_tree_nodes) in child_tree_nodess.into_iter().enumerate() {
                let mut child_tree_node_position = pt_pos.clone();
                child_tree_node_position.push(s_pos);
                execution_stack.push((
                    child_tree_nodes.values().cloned().collect(),
                    child_tree_node_position,
                ));
            }

            tree.insert(pt_pos, rule);
        }
        // }

        tree
    }
}

/// Analyzes some of the components in the composition of a rule and returns
/// * a map that assigns a new successor index to the variables in the composition components, and
/// * an iterator over the old successor indices filtered by occurrence in the given components.
fn successors_used_in_comps<'a, N, T, W>(
    rule: &'a PMCFGRule<N, T, W>,
    comps: &'a FnvHashSet<u8>,
) -> (FnvHashMap<u8, u8>, impl Iterator<Item = u8>)
where
    N: Clone,
{
    let successors = rule
        .composition
        .composition
        .iter()
        .enumerate()
        .filter_map(|(c, v)| {
            if comps.contains(&(c as u8)) {
                Some(v)
            } else {
                None
            }
        })
        .flat_map(|v| {
            v.iter().filter_map(|x| match *x {
                VarT::Var(i, _) => Some(i as u8),
                _ => None,
            })
        })
        .collect::<BTreeSet<u8>>();

    (
        successors
            .iter()
            .enumerate()
            .map(|(i, j)| (*j as u8, i as u8))
            .collect(),
        successors.into_iter(),
    )
}

/// Changes the first index of all Variables according to `reordering` and adds `offset`.
fn reorder_successors<'a, T>(
    component: &'a [VarT<T>],
    reordering: &'a FnvHashMap<u8, u8>,
    offset: u8,
) -> impl Iterator<Item = VarT<T>> + 'a
where
    T: Clone + 'a,
{
    component.iter().map(move |s| match s {
        &VarT::Var(i, j) => VarT::Var(
            *reordering.get(&(i as u8)).unwrap() as usize + offset as usize,
            j,
        ),
        t => t.clone(),
    })
}

/// Merges a set of rules according to the given set of components in the fashion described in the function `FailedParseTree::merge`.
/// The iterator `nodes` enumerates tuples consisting of
/// * a set of component indices,
/// * a grammar rule, and
/// * a child mapping of `FailedParseTreeNode` (successor -> rule -> node_id).
/// It returns
/// * a constructed grammar rule with weight 0, and
/// * a sorted list of maps (rule -> node_id) for each child with occuring variable in the components.
fn merge_rules<'a, 'b, N, T, W>(
    nodes: impl Iterator<
        Item = (
            &'a FnvHashSet<u8>,
            &'b PMCFGRule<N, T, W>,
            &'a BTreeMap<u8, FnvHashMap<u32, usize>>,
        ),
    >,
) -> (PMCFGRule<N, T, W>, Vec<&'a FnvHashMap<u32, usize>>)
where
    'b: 'a,
    N: Clone + 'b,
    T: Clone + 'b,
    W: Copy + Zero + 'b,
{
    let mut heads = Vec::new();
    let mut successors = Vec::new();
    let mut successor_tree_nodes = Vec::new();
    let mut composition: Vec<Vec<VarT<T>>> = Vec::new();

    // sort by first component of used rule TODO: remove this
    let mut nodev = nodes.collect::<Vec<_>>();
    nodev.sort_by(|&(set, _, _), &(set2, _, _)| {
        set.iter().min().unwrap().cmp(set2.iter().min().unwrap())
    });

    for (components, rule, successor_trees) in nodev {
        let (ordering, succ_indices) = successors_used_in_comps(rule, components);
        heads.push(rule.head.clone());
        for c in components {
            VecMultiMapAdapter(&mut composition)[*c as usize].extend(reorder_successors(
                &rule.composition.composition[*c as usize],
                &ordering,
                successors.len() as u8,
            ));
        }

        for successor_index in succ_indices {
            successors.push(rule.tail[successor_index as usize].clone());
            successor_tree_nodes.push(successor_trees.get(&successor_index).unwrap())
        }
    }

    (
        PMCFGRule {
            head: heads.into_iter().next().unwrap(),
            tail: successors,
            weight: W::zero(),
            composition: Composition { composition },
        },
        successor_tree_nodes,
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use log_domain::LogDomain;

    #[test]
    fn rule_merging() {
        use self::VarT::*;
        let one: LogDomain<f64> = LogDomain::one();

        let rules: Vec<(PMCFGRule<char, &str, _>, FnvHashSet<u8>)> = vec![
            (
                PMCFGRule {
                    head: 'A',
                    tail: vec!['B', 'C'],
                    weight: one,
                    composition: Composition {
                        composition: vec![vec![Var(1, 0)], vec![Var(0, 0)]],
                    },
                },
                vec![0u8].into_iter().collect(),
            ),
            (
                PMCFGRule {
                    head: 'A',
                    tail: vec!['D', 'E'],
                    weight: one,
                    composition: Composition {
                        composition: vec![vec![], vec![Var(0, 0), Var(1, 0)]],
                    },
                },
                vec![1u8].into_iter().collect(),
            ),
        ];

        let maps: Vec<BTreeMap<u8, FnvHashMap<u32, usize>>> = vec![
            vec![
                (0u8, vec![(1u32, 1)].into_iter().collect()),
                (1u8, vec![(2u32, 2)].into_iter().collect()),
            ]
            .into_iter()
            .collect(),
            vec![
                (0u8, vec![(3u32, 3)].into_iter().collect()),
                (1u8, vec![(4u32, 4)].into_iter().collect()),
            ]
            .into_iter()
            .collect(),
        ];

        assert_eq!(
            merge_rules(
                rules
                    .iter()
                    .enumerate()
                    .map(|(i, &(ref a, ref b))| (b, a, &maps[i]))
            ),
            (
                PMCFGRule {
                    head: 'A',
                    tail: vec!['C', 'D', 'E'],
                    weight: one,
                    composition: Composition {
                        composition: vec![vec![Var(0, 0)], vec![Var(1, 0), Var(2, 0)]]
                    }
                },
                vec![
                    maps[0].get(&1).unwrap(),
                    &maps[1].get(&0).unwrap(),
                    &maps[1].get(&1).unwrap()
                ]
            )
        )
    }

    #[test]
    fn readoff() {
        assert_eq!(
            FailedParseTree::new(&fails_with_fallbacks()[0].0).0,
            vec![
                (
                    0,
                    FailedParseTreeNode {
                        rule: 0,
                        components: vec![0].into_iter().collect(),
                        children: vec![(0, vec![(1, 1), (2, 2)].into_iter().collect())]
                            .into_iter()
                            .collect()
                    }
                ),
                (
                    1,
                    FailedParseTreeNode {
                        rule: 1,
                        components: vec![0].into_iter().collect(),
                        children: BTreeMap::new()
                    }
                ),
                (
                    2,
                    FailedParseTreeNode {
                        rule: 2,
                        components: vec![1].into_iter().collect(),
                        children: BTreeMap::new()
                    }
                )
            ]
            .into_iter()
            .collect::<IntMap<_>>()
        )
    }

    #[test]
    fn tree_merging() {
        for (word, _, tree) in fails_with_fallbacks() {
            assert_eq!(FailedParseTree::new(&word).merge(&rules()), tree);
        }
    }

    fn rules() -> Vec<PMCFGRule<char, &'static str, LogDomain<f64>>> {
        use self::VarT::*;
        let one: LogDomain<f64> = LogDomain::one();

        let mut vec = Vec::new();

        vec.push(PMCFGRule {
            head: 'S',
            tail: vec!['A'],
            composition: Composition {
                composition: vec![vec![Var(0, 0), Var(0, 1)]],
            },
            weight: one,
        });

        vec.push(PMCFGRule {
            head: 'A',
            tail: Vec::new(),
            composition: Composition {
                composition: vec![vec![], vec![T("asdf")]],
            },
            weight: one,
        });
        vec.push(PMCFGRule {
            head: 'A',
            tail: Vec::new(),
            composition: Composition {
                composition: vec![vec![T("qwer")], vec![]],
            },
            weight: one,
        });

        vec.push(PMCFGRule {
            head: 'A',
            tail: vec!['B', 'C'],
            composition: Composition {
                composition: vec![vec![Var(1, 1)], vec![]],
            },
            weight: one,
        });
        vec.push(PMCFGRule {
            head: 'A',
            tail: vec!['D', 'E'],
            composition: Composition {
                composition: vec![vec![], vec![Var(1, 1)]],
            },
            weight: one,
        });
        vec.push(PMCFGRule {
            head: 'C',
            tail: vec!['A'],
            composition: Composition {
                composition: vec![vec![Var(0, 0)], vec![Var(0, 1)]],
            },
            weight: one,
        });
        vec.push(PMCFGRule {
            head: 'E',
            tail: vec!['A'],
            composition: Composition {
                composition: vec![vec![Var(0, 1)], vec![Var(0, 0)]],
            },
            weight: one,
        });

        vec
    }

    fn fails_with_fallbacks() -> Vec<(
        Vec<Delta>,
        usize,
        GornTree<PMCFGRule<char, &'static str, LogDomain<f64>>>,
    )> {
        use self::Bracket::*;
        use self::BracketContent::*;
        use self::VarT::*;
        let one: LogDomain<f64> = LogDomain::one();
        let rules = rules();

        vec![
            (
                vec![
                    Open(Component(0, 0)),
                    Open(Variable(0, 0, 0)),
                    Open(Component(1, 0)),
                    Close(Component(1, 0)),
                    Close(Variable(0, 0, 0)),
                    Open(Variable(0, 0, 1)),
                    Open(Component(2, 0)),
                    Close(Component(2, 0)),
                    Close(Variable(0, 0, 1)),
                    Close(Component(0, 0)),
                ],
                1,
                vec![
                    (vec![], rules[0].clone()),
                    (
                        vec![0],
                        PMCFGRule {
                            head: 'A',
                            tail: vec![],
                            composition: Composition {
                                composition: vec![vec![], vec![]],
                            },
                            weight: one,
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            ),
            (
                vec![
                    Open(Component(0, 0)),
                    Open(Variable(0, 0, 0)),
                    Open(Component(3, 0)),
                    Open(Variable(3, 1, 1)),
                    Open(Component(5, 1)),
                    Open(Variable(5, 0, 1)),
                    Open(Component(2, 1)),
                    Close(Component(2, 1)),
                    Close(Variable(5, 0, 1)),
                    Close(Component(5, 1)),
                    Close(Variable(3, 1, 1)),
                    Close(Component(3, 0)),
                    Close(Variable(0, 0, 0)),
                    Open(Variable(0, 0, 1)),
                    Open(Component(4, 1)),
                    Open(Variable(4, 1, 1)),
                    Open(Component(6, 1)),
                    Open(Variable(6, 0, 0)),
                    Open(Component(1, 0)),
                    Close(Component(1, 0)),
                    Close(Variable(6, 0, 0)),
                    Close(Component(6, 1)),
                    Close(Variable(4, 1, 1)),
                    Close(Component(4, 0)),
                    Close(Variable(0, 0, 1)),
                    Close(Component(0, 0)),
                ],
                1,
                vec![
                    (vec![], rules[0].clone()),
                    (
                        vec![0],
                        PMCFGRule {
                            head: 'A',
                            tail: vec!['C', 'E'],
                            composition: Composition {
                                composition: vec![vec![Var(0, 1)], vec![Var(1, 1)]],
                            },
                            weight: one,
                        },
                    ),
                    (
                        vec![0, 0],
                        // rules.find_value(5).unwrap().clone()
                        PMCFGRule {
                            head: 'C',
                            tail: vec!['A'],
                            composition: Composition {
                                composition: vec![vec![], vec![Var(0, 1)]],
                            },
                            weight: one,
                        },
                    ),
                    (
                        vec![0, 0, 0],
                        // rules.find_value(2).unwrap().clone()
                        PMCFGRule {
                            head: 'A',
                            tail: vec![],
                            composition: Composition {
                                composition: vec![vec![], vec![]],
                            },
                            weight: one,
                        },
                    ),
                    (
                        vec![0, 1],
                        // rules.find_value(6).unwrap().clone()
                        PMCFGRule {
                            head: 'E',
                            tail: vec!['A'],
                            composition: Composition {
                                composition: vec![vec![], vec![Var(0, 0)]],
                            },
                            weight: one,
                        },
                    ),
                    (
                        vec![0, 1, 0],
                        // rules.find_value(1).unwrap().clone()
                        PMCFGRule {
                            head: 'A',
                            tail: vec![],
                            composition: Composition {
                                composition: vec![vec![]],
                            },
                            weight: one,
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            ),
        ]
    }

    #[test]
    fn fails() {
        for (word, fails, _) in fails_with_fallbacks() {
            assert_eq!(FailedParseTree::new(&word).fails(), fails)
        }
    }
}
