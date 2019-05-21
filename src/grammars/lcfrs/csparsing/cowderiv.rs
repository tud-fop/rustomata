use super::{Bracket, BracketContent, Delta, PMCFGRule};
use crate::{grammars::pmcfg::VarT, util::tree::GornTree};
use num_traits::Zero;
use std::collections::HashMap;
use vecmultimap::VecMultiMapAdapter;

#[derive(PartialEq, Debug)]
pub struct LabelledTreeNode<L, I> {
    content: I,
    successors: Vec<(L, LabelledTreeNode<L, I>)>,
}

impl<I: PartialEq> LabelledTreeNode<(usize, usize), I> {
    /// Checks consistency of a cow derivation.
    /// The check implemented in csparsing::toderiv is slightly faster,
    /// since it does not need to convert the given word into a cow
    /// derivation.
    #[allow(dead_code)]
    pub fn is_consistent(&self) -> bool {
        Self::is_consistent_vec(vec![self])
    }

    fn is_consistent_vec(trees: Vec<&Self>) -> bool {
        // check if all elements are equal
        if let Some((t, ts)) = trees.split_first() {
            if !ts.iter().all(|te| te.content == t.content) {
                return false;
            }
        }

        // collect successors
        let mut successors_per_fst: Vec<Vec<&Self>> = Vec::new();
        for t in trees {
            for &((i, _), ref successor) in &t.successors {
                VecMultiMapAdapter(&mut successors_per_fst).push_to(i, successor);
            }
        }

        // consistency for successors
        successors_per_fst.into_iter().all(Self::is_consistent_vec)
    }
}

pub type CowDerivation = LabelledTreeNode<(usize, usize), usize>;
type NtReindex = HashMap<(usize, usize), usize>;

impl CowDerivation {
    pub fn new(v: &[Delta]) -> Self {
        use self::{Bracket::*, BracketContent::*};

        let mut root: CowDerivation = LabelledTreeNode {
            content: 0,
            successors: Vec::new(),
        };
        let mut pos: Vec<*mut CowDerivation> = vec![&mut root as *mut CowDerivation];

        for symbol in v {
            match *symbol {
                Open(Component(rule, _)) => unsafe {
                    (**pos.last().unwrap()).content = rule as usize;
                },
                Open(Variable(_, i, j)) => unsafe {
                    (**pos.last().unwrap()).successors.push((
                        (i as usize, j as usize),
                        LabelledTreeNode {
                            content: 0,
                            successors: Vec::new(),
                        },
                    ));
                    let p = &mut (**pos.last().unwrap()).successors.last_mut().unwrap().1
                        as *mut CowDerivation;
                    pos.push(p);
                },
                Close(Variable(_, _, _)) => {
                    pos.pop();
                }
                _ => {}
            }
        }

        root
    }

    /// Reads a derivation from a consistent cow derivation;
    /// fails if the given cow derivation is not constistent.
    // The read-off implemented in csparsing::toderiv is still slightly faster,
    /// since it does not need to convert the given word into a cow
    /// derivation.
    #[allow(dead_code)]
    pub fn to_mcfg_deriv<'a, N: 'a, T: 'a, W: 'a>(
        &self,
        int: &'a [PMCFGRule<N, T, W>],
    ) -> Option<GornTree<&'a PMCFGRule<N, T, W>>> {
        let mut tree = GornTree::new();

        if Self::cows_to_mcfg_deriv(&[self], int, &mut tree, Vec::new()) {
            Some(tree)
        } else {
            None
        }
    }

    fn cows_to_mcfg_deriv<'a, N, T, W>(
        trees: &[&Self],
        int: &'a [PMCFGRule<N, T, W>],
        deriv: &mut GornTree<&'a PMCFGRule<N, T, W>>,
        pos: Vec<usize>,
    ) -> bool {
        // check if all elements are equal
        if let Some((t, ts)) = trees.split_first() {
            if !ts.iter().all(|te| te.content == t.content) {
                return false;
            }
        }

        // collect successors
        let mut successors_per_fst: Vec<Vec<&Self>> = Vec::new();
        for t in trees {
            for &((i, _), ref successor) in &t.successors {
                VecMultiMapAdapter(&mut successors_per_fst).push_to(i, successor);
            }
        }

        // consistency for successors
        let toderiv_with_pos = |(idx, group): (usize, Vec<&Self>)| -> bool {
            let mut spos = pos.clone();
            spos.push(idx);
            Self::cows_to_mcfg_deriv(&group, int, deriv, spos)
        };

        if successors_per_fst
            .into_iter()
            .enumerate()
            .all(toderiv_with_pos)
        {
            deriv.insert(pos, &int[trees[0].content]);
            true
        } else {
            false
        }
    }

    /// Collects the cildren of each cow derivation in the given iterator
    /// and groups them all by the first index of the outgoing edge label.
    /// The returned groups consists of tuples containing
    /// * the second edge label, and
    /// * a reference to the child cow derivation.
    fn collect_children<'a, I>(trees: I, reindex: NtReindex) -> Vec<Vec<(usize, &'a Self)>>
    where
        I: Iterator<Item = &'a Self>,
    {
        let mut successors_per_fst: Vec<Vec<(usize, &Self)>> = Vec::new();

        for t in trees {
            for &((i, j), ref successor) in &t.successors {
                let index = reindex[&(t.content, i)];
                VecMultiMapAdapter(&mut successors_per_fst).push_to(index, (j, successor));
            }
        }

        successors_per_fst
    }

    /// Constructs a derivation from a given cow derivation.
    pub fn fallback<N, T, W>(&self, int: &[PMCFGRule<N, T, W>]) -> GornTree<PMCFGRule<N, T, W>>
    where
        N: Clone,
        T: Clone,
        W: Zero,
    {
        let mut deriv = GornTree::new();
        Self::fallback_vec(&[(0, self)], int, &mut deriv, Vec::new());
        deriv
    }

    /// Merge a list of rules given by an iterator over triples containing
    /// * an index in the component,
    /// * a unique rule id, and
    /// * the rules.
    /// Returns a reordering of rhs nonterminals (ruleid, rhs index ↦ new rhs index)
    /// and the constructed rule.
    fn merge_rules<'a, N, T, W, I>(head: N, roots: I) -> (NtReindex, PMCFGRule<N, T, W>)
    where
        W: 'a + Zero,
        N: 'a + Clone,
        T: 'a + Clone,
        I: Clone + Iterator<Item = (usize, usize, &'a PMCFGRule<N, T, W>)>,
    {
        // store iterator for a second pass
        let second_pass = roots.clone();

        // first pass through compositions:
        // collects all used nonterminals and reorders them by occurance
        let mut nt_reindex = HashMap::new();
        let mut tail = Vec::new();
        let mut fanout = 0;
        for (component, ruleid, rule) in roots {
            for i in rule.composition[component]
                .iter()
                .filter_map(|symbol| match *symbol {
                    VarT::Var(i, _) => Some(i),
                    _ => None,
                })
            {
                nt_reindex.entry((ruleid, i)).or_insert_with(|| {
                    tail.push(rule.tail[i].clone());
                    tail.len() - 1
                });
            }

            fanout = usize::max(fanout, component + 1);
        }

        // second pass through compositions:
        // uses reordering of nonterminals and applies it
        // to all variables' first indices
        let mut composition = Vec::new();
        composition.resize_with(fanout, Vec::new);
        for (component, ruleid, rule) in second_pass {
            composition[component].extend(rule.composition[component].iter().map(|symbol| {
                match symbol {
                    VarT::T(t) => VarT::T(t.clone()),
                    &VarT::Var(i, j) => VarT::Var(nt_reindex[&(ruleid, i)], j),
                }
            }));
        }

        (
            nt_reindex,
            PMCFGRule {
                head,
                tail,
                composition: composition.into(),
                weight: W::zero(),
            },
        )
    }

    // Merges a group of cow derivations.
    fn fallback_vec<N, T, W>(
        roots: &[(usize, &Self)],
        int: &[PMCFGRule<N, T, W>],
        tree: &mut GornTree<PMCFGRule<N, T, W>>,
        pos: Vec<usize>,
    ) where
        N: Clone,
        T: Clone,
        W: Zero,
    {
        let root_lhs = int[roots[0].1.content].head.clone();
        let (nt_reindex, root_node) = Self::merge_rules(
            root_lhs,
            roots
                .iter()
                .map(|&(j, ref t)| (j, t.content, &int[t.content])),
        );

        // process children with same first label
        for (sidx, child_group) in Self::collect_children(roots.iter().map(|&(_, t)| t), nt_reindex)
            .into_iter()
            .enumerate()
            .filter(|&(_, ref v)| !v.is_empty())
        {
            let mut spos = pos.clone();
            spos.push(sidx);
            Self::fallback_vec(&child_group, int, tree, spos);
        }

        tree.insert(pos, root_node);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::grammars::pmcfg::Composition;
    use log_domain::LogDomain;
    use num_traits::One;

    #[test]
    fn rule_merging() {
        use self::VarT::*;
        let one: LogDomain<f64> = LogDomain::one();

        let rules: Vec<(usize, usize, PMCFGRule<char, char, LogDomain<f64>>)> = vec![
            (
                0,
                0,
                PMCFGRule {
                    head: 'A',
                    tail: vec!['B', 'C'],
                    weight: one,
                    composition: Composition {
                        composition: vec![vec![Var(1, 0)], vec![Var(0, 0)]],
                    },
                },
            ),
            (
                1,
                1,
                PMCFGRule {
                    head: 'A',
                    tail: vec!['D', 'E'],
                    weight: one,
                    composition: Composition {
                        composition: vec![vec![], vec![Var(0, 0), Var(1, 0)]],
                    },
                },
            ),
        ];

        assert_eq!(
            CowDerivation::merge_rules('A', rules.iter().map(|&(c, rid, ref r)| (c, rid, r))).1,
            PMCFGRule {
                head: 'A',
                tail: vec!['C', 'D', 'E'],
                weight: LogDomain::zero(),
                composition: Composition {
                    composition: vec![vec![Var(0, 0)], vec![Var(1, 0), Var(2, 0)]]
                }
            }
        )
    }

    #[test]
    fn readoff() {
        for (word, cowd, _) in fails_with_fallbacks() {
            assert_eq!(CowDerivation::new(&word), cowd)
        }
    }

    #[test]
    fn tree_merging() {
        for (word, _, tree) in fails_with_fallbacks() {
            assert_eq!(CowDerivation::new(&word).fallback(&rules()), tree);
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
        CowDerivation,
        GornTree<PMCFGRule<char, &'static str, LogDomain<f64>>>,
    )> {
        use self::Bracket::*;
        use self::BracketContent::*;
        use self::VarT::*;
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
                LabelledTreeNode {
                    content: 0,
                    successors: vec![
                        (
                            (0, 0),
                            LabelledTreeNode {
                                content: 1,
                                successors: vec![],
                            },
                        ),
                        (
                            (0, 1),
                            LabelledTreeNode {
                                content: 2,
                                successors: vec![],
                            },
                        ),
                    ],
                },
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
                            weight: LogDomain::zero(),
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
                LabelledTreeNode {
                    content: 0,
                    successors: vec![
                        (
                            (0, 0),
                            LabelledTreeNode {
                                content: 3,
                                successors: vec![(
                                    (1, 1),
                                    LabelledTreeNode {
                                        content: 5,
                                        successors: vec![(
                                            (0, 1),
                                            LabelledTreeNode {
                                                content: 2,
                                                successors: vec![],
                                            },
                                        )],
                                    },
                                )],
                            },
                        ),
                        (
                            (0, 1),
                            LabelledTreeNode {
                                content: 4,
                                successors: vec![(
                                    (1, 1),
                                    LabelledTreeNode {
                                        content: 6,
                                        successors: vec![(
                                            (0, 0),
                                            LabelledTreeNode {
                                                content: 1,
                                                successors: vec![],
                                            },
                                        )],
                                    },
                                )],
                            },
                        ),
                    ],
                },
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
                            weight: LogDomain::zero(),
                        },
                    ),
                    (
                        vec![0, 0],
                        PMCFGRule {
                            head: 'C',
                            tail: vec!['A'],
                            composition: Composition {
                                composition: vec![vec![], vec![Var(0, 1)]],
                            },
                            weight: LogDomain::zero(),
                        },
                    ),
                    (
                        vec![0, 0, 0],
                        PMCFGRule {
                            head: 'A',
                            tail: vec![],
                            composition: Composition {
                                composition: vec![vec![], vec![]],
                            },
                            weight: LogDomain::zero(),
                        },
                    ),
                    (
                        vec![0, 1],
                        PMCFGRule {
                            head: 'E',
                            tail: vec!['A'],
                            composition: Composition {
                                composition: vec![vec![], vec![Var(0, 0)]],
                            },
                            weight: LogDomain::zero(),
                        },
                    ),
                    (
                        vec![0, 1, 0],
                        PMCFGRule {
                            head: 'A',
                            tail: vec![],
                            composition: Composition {
                                composition: vec![vec![]],
                            },
                            weight: LogDomain::zero(),
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            ),
        ]
    }

    // #[test]
    // fn fails() {
    //     for (word, fails, _) in fails_with_fallbacks() {
    //         assert_eq!(FailedParseTree::new(&word).fails(), fails)
    //     }
    // }
}
