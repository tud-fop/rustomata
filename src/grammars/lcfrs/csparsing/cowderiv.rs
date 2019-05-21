#![allow(dead_code)]

use vecmultimap::VecMultiMapAdapter;
use crate::{util::tree::GornTree, grammars::pmcfg::VarT};
use super::{PMCFGRule, Delta, Bracket, BracketContent};
use num_traits::Zero;
use std::collections::HashMap;

#[derive(PartialEq, Debug)]
pub struct LabelledTreeNode<L, I> {
    content: I,
    successors: Vec<(L, LabelledTreeNode<L, I>)>
}

impl<I: PartialEq> LabelledTreeNode<(usize, usize), I> {
    pub fn is_consistent(&self) -> bool {
        Self::is_consistent_vec(vec![self])
    }

    fn is_consistent_vec(trees: Vec<&Self>) -> bool {
        // check if all elements are equal
        if let Some((t, ts)) = trees.split_first() {
            if ! ts.iter().all(|te| te.content == t.content) {
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
        successors_per_fst.into_iter().all(|v| Self::is_consistent_vec(v))
    }
}

pub type CowDerivation = LabelledTreeNode<(usize, usize), usize>;

impl CowDerivation {
    pub fn new(v: &[Delta]) -> Self {
        use self::{Bracket::*, BracketContent::*};

        let mut root: CowDerivation = LabelledTreeNode{ content: 0, successors: Vec::new() };
        let mut pos: Vec<*mut CowDerivation> = vec![&mut root as *mut CowDerivation];
        
        for symbol in v {
            match *symbol {
                Open(Component(rule, _)) => unsafe {
                    (**pos.last().unwrap()).content = rule as usize;
                },
                Open(Variable(_, i, j)) => unsafe {
                    (**pos.last().unwrap()).successors.push(((i as usize, j as usize), LabelledTreeNode{ content: 0, successors: Vec::new() }));
                    let p = &mut (**pos.last().unwrap()).successors.last_mut().unwrap().1 as *mut CowDerivation;
                    pos.push(p);
                },
                Close(Variable(_, _, _)) => { pos.pop(); },
                _ => {}
            }
        }

        root
    }

    pub fn as_mcfg_deriv<'a, N: 'a , T: 'a , W: 'a>(&self, int: &'a [PMCFGRule<N, T, W>]) -> Option<GornTree<&'a PMCFGRule<N, T, W>>> {
        let mut tree = GornTree::new();
        
        if Self::as_mcfg_deriv_vec(vec![self], int, &mut tree, Vec::new()) {
            Some(tree)
        } else {
            None
        }
    }

    fn as_mcfg_deriv_vec<'a, N, T, W>(trees: Vec<&Self>, int: &'a [PMCFGRule<N, T, W>], deriv: &mut GornTree<&'a PMCFGRule<N, T, W>>, pos: Vec<usize>) -> bool {
        // check if all elements are equal
        if let Some((t, ts)) = trees.split_first() {
            if ! ts.iter().all(|te| te.content == t.content) {
                return false;
            }
        }

        // collect successors
        let mut successors_per_fst: Vec<Vec<&Self>> = Vec::new();
        for t in &trees {
            for &((i, _), ref successor) in &t.successors {
                VecMultiMapAdapter(&mut successors_per_fst).push_to(i, successor);
            }
        }

        // consistency for successors
        if successors_per_fst.into_iter().enumerate().all(|(i, v)| { let mut spos = pos.clone(); spos.push(i); Self::as_mcfg_deriv_vec(v, int, deriv, spos) }) {
            deriv.insert(pos, &int[trees[0].content]);
            true
        } else {
            false
        }
    }

    fn collect_children<'a, 'b, 'c>(roots: &'a [(usize, &'b Self)], nt_offset_per_rule: HashMap<usize, usize>, index_after_removal: Vec<usize>) -> Vec<Vec<(usize, &'c Self)>>
    where
        'a: 'c,
        'b: 'c
    {
        let mut successors_per_fst: Vec<Vec<(usize, &Self)>> = Vec::new();
        
        for &(_, ref t) in roots {
            for &((i, j), ref successor) in &t.successors {
                let index = index_after_removal[i + nt_offset_per_rule[&t.content]];
                VecMultiMapAdapter(&mut successors_per_fst).push_to(index, (j, successor));
            }
        }
        
        successors_per_fst
    }

    pub fn fallback<N, T, W>(&self, int: &[PMCFGRule<N, T, W>]) -> GornTree<PMCFGRule<N, T, W>>
    where
        N: Clone,
        T: Clone,
        W: Zero
    {
        let mut deriv = GornTree::new();
        Self::fallback_vec(&[(0, self)], int, &mut deriv, Vec::new());
        deriv
    }

    fn merge_rules<N, T, W>(head: N, roots: &[(usize, &Self)], int: &[PMCFGRule<N, T, W>]) -> (HashMap<usize, usize>, Vec<usize>, PMCFGRule<N, T, W>)
    where
        W: Zero,
        N: Clone,
        T: Clone
    {
        use std::collections::HashSet;
        use std::iter::{repeat, repeat_with};
        use std::mem::replace;

        // concatenate right-hand sides of rules:
        // * collect all rhs nonterminals of all rules
        //   (skip for already processed rules)
        // * collect composition components, change first
        //   index according to index in list of
        //   collected nonterminals
        let mut used_nts: Vec<bool> = Vec::new();
        let mut used_nt_count = 0;
        let mut fanout = 0;
        let mut nt_offset_per_rule = HashMap::new();
        for &(component, t) in roots.iter() {
            let offset = *nt_offset_per_rule.entry(t.content).or_insert_with(
                || {
                    let offset = used_nts.len();
                    used_nts.extend(repeat(false).take(int[t.content].tail.len()));
                    offset
                }
            );

            for i in roots.iter().flat_map(|&(_, t)| &int[t.content].composition[component])
                                 .filter_map(|symbol| match *symbol { VarT::Var(i, _) => Some(i) , _ => None }) {
                if !replace(&mut used_nts[i+offset], true) {
                    used_nt_count += 1;
                }
            }

            fanout = usize::max(fanout, component + 1);
        }
        
        // remove unused nonterminals, store new index
        let mut index_after_removal = vec![0; used_nts.len()];
        let mut composition = Vec::new();
        composition.resize_with(fanout, Vec::new);
        let mut tail = Vec::with_capacity(used_nt_count);
        let mut processed_rules = HashSet::new();
        let mut nt_index = 0;

        for &(component, ref t) in roots {
            let nt_offset = nt_offset_per_rule[&t.content];
            let nts = int[t.content].tail.len();
            let used_nts_range = nt_offset .. nt_offset+nts;

            if processed_rules.insert(t.content) {
                tail.extend(int[t.content].tail.iter().enumerate().zip(&used_nts[used_nts_range])
                    .filter_map(
                        |((nti, nt), b)|
                        if *b {
                            index_after_removal[nt_offset + nti] = nt_index;
                            nt_index += 1;
                            Some(nt)
                        } else {
                            None
                        }
                    ).cloned());
            }
            
            composition[component].extend(
                int[t.content].composition[component].iter().map(
                    |symbol|
                    match symbol {
                        VarT::T(t) => VarT::T(t.clone()),
                        &VarT::Var(i, j) => VarT::Var(index_after_removal[nt_offset + i], j)
                    }
                )
            );
        }
    
        (
            nt_offset_per_rule,
            index_after_removal,
            PMCFGRule{ head, tail, composition: composition.into(), weight: W::zero() }
        )
    }

    fn fallback_vec<N, T, W>(roots: &[(usize, &Self)], int: &[PMCFGRule<N, T, W>], tree: &mut GornTree<PMCFGRule<N, T, W>>, pos: Vec<usize>)
    where
        N: Clone,
        T: Clone,
        W: Zero
    {
        let root_lhs = int[roots[0].1.content].head.clone();
        let (nt_offset, nt_removal, root_node) = Self::merge_rules(root_lhs, roots, int);
        
        // process children with same first label
        for (sidx, child_group) in Self::collect_children(roots, nt_offset, nt_removal).into_iter().enumerate().filter(|&(_, ref v)| !v.is_empty()) {
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

        let rules: Vec<PMCFGRule<char, char, LogDomain<f64>>> = vec![
            PMCFGRule {
                head: 'A',
                tail: vec!['B', 'C'],
                weight: one,
                composition: Composition {
                    composition: vec![vec![Var(1, 0)], vec![Var(0, 0)]],
                },
            },
            PMCFGRule {
                head: 'A',
                tail: vec!['D', 'E'],
                weight: one,
                composition: Composition {
                    composition: vec![vec![], vec![Var(0, 0), Var(1, 0)]],
                },
            }
        ];

        let treestub1 = LabelledTreeNode{ content: 0, successors: vec![] };
        let treestub2 = LabelledTreeNode{ content: 1, successors: vec![] };
        
        let treestubs = vec![
            (0, &treestub1),
            (1, &treestub2)
        ];
        
        assert_eq!(
            CowDerivation::merge_rules('A', &treestubs, &rules).2,
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
                LabelledTreeNode {
                    content: 0,
                    successors: vec![
                        ((0, 0), LabelledTreeNode{ content: 1, successors: vec![] }),
                        ((0, 1), LabelledTreeNode{ content: 2, successors: vec![] })
                    ]
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
                        ((0, 0), LabelledTreeNode{
                            content: 3,
                            successors: vec![
                                ((1, 1), LabelledTreeNode{
                                    content: 5,
                                    successors: vec![
                                        ((0, 1), LabelledTreeNode{ content: 2, successors: vec![] })
                                    ]
                                })
                            ]
                        }),
                        ((0, 1), LabelledTreeNode{
                            content: 4,
                            successors: vec![
                                ((1, 1), LabelledTreeNode{
                                    content: 6,
                                    successors: vec![
                                        ((0, 0), LabelledTreeNode{
                                            content: 1,
                                            successors: vec![]
                                        })
                                    ]
                                })
                            ]
                        })
                    ]
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
