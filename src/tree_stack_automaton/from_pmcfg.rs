extern crate num_traits;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::{self, Display, Formatter};
use std::hash::Hash;
use std::iter::FromIterator;
use std::vec::Vec;
use self::num_traits::One;
use recognisable::Transition;
use pmcfg::{PMCFG, PMCFGRule, VarT};
use tree_stack_automaton::{TreeStack, TreeStackAutomaton, TreeStackInstruction};

// types for analysis of derivation trees of PMCFGs
pub type RuleCallerMap<N, T, W> = BTreeMap<PMCFGRule<N, T, W>, Vec<(usize, Vec<T>)>>;
pub type DerivationSnippet<N, T, W> = (PMCFGRule<N, T, W>, Vec<Vec<PMCFGRule<N, T, W>>>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum PosState<X> {
    Designated,
    Initial,
    Position(X, usize, usize),
}

impl<X> PosState<X> {
    pub fn map<F, Y>(&self, f: F) -> PosState<Y>
        where F: Fn(&X) -> Y,
    {
        match *self {
            PosState::Designated => PosState::Designated,
            PosState::Initial => PosState::Initial,
            PosState::Position(ref x, i, j) => PosState::Position(f(x), i, j),
        }
    }
}

impl<X: Display> Display for PosState<X> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            PosState::Designated
                => write!(f, "@"),
            PosState::Initial
                => write!(f, "I"),
            PosState::Position(ref x, i, j)
                => write!(f, "({}, {}, {})", x, i, j)
        }
    }
}

// TODO assumes that the PMCFG is monotonic on the visit-order of components
impl<N: Clone + Ord + PartialEq + Hash,
     T: Clone + Ord + PartialEq + Hash,
     W: Clone + Ord + PartialEq + One
     > From<PMCFG<N, T, W>> for TreeStackAutomaton<PosState<PMCFGRule<N, T, W>>, T, W> {
    fn from(g: PMCFG<N, T, W>) -> Self {
        let mut transitions = Vec::new();

        let mut rule_map: BTreeMap<N, Vec<PMCFGRule<N, T, W>>>
            = BTreeSet::from_iter(
                g.rules
                    .iter()
                    .map(|r| r.head.clone())
            )
            .iter()
            .map(|n| (
                n.clone(),
                Vec::<PMCFGRule<N, T, W>>::new()
            ))
            .collect();

        let mut down_info: RuleCallerMap<N, T, W>
            = BTreeMap::new();

        let mut initial_rules: Vec<PMCFGRule<N, T, W>>
            = Vec::new();

        for r in g.rules.clone() {
            if g.initial.contains(&r.head) {
                initial_rules.push(r.clone())
            }
            rule_map.get_mut(&r.head).unwrap().push(r.clone());

            let mut var_cnt_vec = Vec::new();
            let mut i: usize;
            let mut down_buffer: Vec<T> = Vec::new();
            for component in &r.composition.composition {
                i = 0;
                for ntt in component {
                    down_buffer.clear();
                    match *ntt {
                        VarT::Var(_, _)
                            => {
                                i += 1;
                                down_buffer.clear();
                            },
                        VarT::T(ref t)
                            => {
                                down_buffer.push(t.clone());
                            }
                    }
                }
                var_cnt_vec.push((i, down_buffer.clone()));
            }
            down_info.insert(r.clone(), var_cnt_vec);
        }

        for r in initial_rules {
            transitions.push(
                Transition {
                    word: Vec::new(),
                    weight: r.weight.clone(),
                    instruction: TreeStackInstruction::Push {
                        n: 0,
                        current_val: PosState::Initial,
                        new_val: PosState::Position(r.clone(), 0, 0)
                    }
                }
            );

            match down_info[&r][0] {
                (j, ref word) => {
                    transitions.push(
                        Transition {
                            word: word.clone(),
                            weight: W::one(),
                            instruction: TreeStackInstruction::Down {
                                current_val: PosState::Position(r.clone(), 0, j),
                                old_val: PosState::Initial,
                                new_val: PosState::Designated
                            }
                        }
                    );
                }
            }
        }
        // each [r, [r₁, …, rₖ]] on the agenda signifies that r(r₁(…), …, rₖ(…)) is a possible subderivation
        let mut agenda: Vec<DerivationSnippet<N, T, W>>
            = Vec::new();

        for r in g.rules {
            agenda.push(
                ( r.clone(),
                  r.tail.iter().map(|n| rule_map[n].clone()).collect()
                )
            );
        }

        let mut buffer: Vec<T>;

        let mut j: usize;
        let mut k: usize;

        for (r, rss) in agenda {
            j = 0;
            let mut previous_component: Vec<Option<usize>> = rss.iter().map(|_| None).collect();
            for component in r.composition.composition.clone() {
                buffer = Vec::new();
                k = 0;
                for token in component {
                    match token {
                        VarT::Var(i1, j1) => {
                            for ri in &rss[i1] {
                                transitions.push(Transition {
                                    word: buffer.clone(),
                                    weight: match previous_component[i1] {
                                        None => ri.weight.clone(),
                                        Some(_) => W::one(),
                                    },
                                    instruction: match previous_component[i1] {
                                        None => {
                                            TreeStackInstruction::Push {
                                                n: i1,
                                                current_val: PosState::Position(r.clone(), j, k),
                                                new_val: PosState::Position(ri.clone(), j1, 0)
                                            }
                                        },
                                        Some(j0) => {
                                            TreeStackInstruction::Up {
                                                n: i1,
                                                current_val: PosState::Position(r.clone(), j, k),
                                                old_val: PosState::Position(
                                                    ri.clone(),
                                                    j0,
                                                    down_info[ri][j0].0
                                                ),
                                                new_val: PosState::Position(ri.clone(), j1, 0)
                                            }
                                        }
                                    }
                                });

                                transitions.push(Transition {
                                    word: down_info[ri][j1].1.clone(),
                                    weight: W::one(),
                                    instruction: TreeStackInstruction::Down {
                                        current_val: PosState::Position(
                                            ri.clone(),
                                            j1,
                                            down_info[ri][j1].0
                                        ),
                                        old_val: PosState::Position(r.clone(), j, k),
                                        new_val: PosState::Position(r.clone(), j, k + 1)
                                    }
                                });
                            }

                            previous_component[i1] = Some(j1);
                            k += 1;
                            buffer.clear();
                        },

                        VarT::T(t) => {
                            buffer.push(t);
                        }
                    }
                }

                j += 1;
            }
        }

        TreeStackAutomaton::new(
            transitions,
            TreeStack::new(PosState::Initial)
        )
    }
}

pub fn to_abstract_syntax_tree<A>((tree_map, _): (BTreeMap<Vec<usize>, PosState<A>>, Vec<usize>)) -> BTreeMap<Vec<usize>, A> {
    let mut abstract_syntax_tree = BTreeMap::new();

    for (address, pos_state) in tree_map {
        let (curr_root_child, relative_address) = if let Some((first, rest)) = address.split_first() {
            (first.clone(), rest.to_vec())
        } else {
            continue;
        };

        if curr_root_child != 0 {
            continue;
        }

        if let PosState::Position(value, _, _) = pos_state {
            abstract_syntax_tree.insert(relative_address, value);
        } else {
            panic!("The given tree map contains 'designated' or 'initial' nodes that are not the root!");
        }
    }

    abstract_syntax_tree
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_to_abstract_syntax_tree() {
        let mut tree_map = BTreeMap::new();
        tree_map.insert(vec![], PosState::Initial);
        tree_map.insert(vec![0], PosState::Position('a', 0, 0));
        tree_map.insert(vec![0,0], PosState::Position('b', 0, 0));
        tree_map.insert(vec![0,2], PosState::Position('c', 0, 0));
        tree_map.insert(vec![2], PosState::Position('d', 0, 0));

        let mut abstract_syntax_tree = BTreeMap::new();
        abstract_syntax_tree.insert(vec![], 'a');
        abstract_syntax_tree.insert(vec![0], 'b');
        abstract_syntax_tree.insert(vec![2], 'c');

        assert_eq!(abstract_syntax_tree, to_abstract_syntax_tree((tree_map, vec![])));
    }

    #[test]
    fn test_to_abstract_syntax_tree_empty_tree() {
        let tree_map: BTreeMap<_, PosState<u8>> = BTreeMap::new();
        assert_eq!(BTreeMap::new(), to_abstract_syntax_tree((tree_map, vec![])));
    }

    #[test]
    #[should_panic(
        expected="The given tree map contains 'designated' or 'initial' nodes that are not the root!"
    )]
    fn test_to_abstract_syntax_tree_invalid_tree() {
        let mut tree_map: BTreeMap<_, PosState<u8>> = BTreeMap::new();
        tree_map.insert(vec![], PosState::Initial);
        tree_map.insert(vec![0], PosState::Initial);

        to_abstract_syntax_tree((tree_map, vec![]));
    }
}
