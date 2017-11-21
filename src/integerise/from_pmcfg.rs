extern crate num_traits;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;
use std::hash::Hash;
use std::iter::FromIterator;
use std::vec::Vec;
use num_traits::{One, Zero};
use std::ops::{Add, Div};
use std::marker::PhantomData;

use integeriser::{Integeriser, HashIntegeriser};

use automata;
use pmcfg::{PMCFG, PMCFGRule, VarT};
use tree_stack_automaton::*;
use integerise::*;

// TODO assumes that the PMCFG is monotonic on the visit-order of components
impl<N: Clone + fmt::Debug + Ord + PartialEq + Hash,
     T: Clone + fmt::Debug + Ord + PartialEq + Hash,
     W: Clone + fmt::Debug + Ord + PartialEq + One + Add<Output=W> + Div<Output=W> + Zero
     > From<PMCFG<N, T, W>> for IntTreeStackAutomaton<PosState<PMCFGRule<N, T, W>>, T, W> {
    fn from(g: PMCFG<N, T, W>) -> Self {
        let mut inter1 = HashIntegeriser::new();
        let mut inter2 = HashIntegeriser::new();

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
            let t = automata::Transition {
                _dummy: PhantomData,
                word: Vec::new(),
                weight: r.weight.clone(),
                instruction: TreeStackInstruction::Push {
                    n: 0,
                    current_val: PosState::Initial,
                    new_val: PosState::Position(r.clone(), 0, 0)
                }
            };
            transitions.push(
                t.integerise(&mut inter1, &mut inter2)
            );

            match down_info[&r][0] {
                (j, ref word) => {
                    let t = automata::Transition {
                        _dummy: PhantomData,
                        word: word.clone(),
                        weight: W::one(),
                        instruction: TreeStackInstruction::Down {
                            current_val: PosState::Position(r.clone(), 0, j),
                            old_val: PosState::Initial,
                            new_val: PosState::Designated
                        }
                    };
                    transitions.push(
                        t.integerise(&mut inter1, &mut inter2)
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
                                let t = automata::Transition {
                                    _dummy: PhantomData,
                                    word: buffer.clone(),
                                    weight: match previous_component[i1] {
                                        None => ri.weight.clone(),
                                        Some(_) => W::one()
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
                                };
                                transitions.push(
                                    t.integerise(&mut inter1, &mut inter2)
                                );

                                let t2 = automata::Transition {
                                    _dummy: PhantomData,
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
                                };
                                transitions.push(
                                    t2.integerise(&mut inter1, &mut inter2)
                                );

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

        let t_auto = TreeStackAutomaton::new(
            transitions,
            TreeStack::new(inter1.integerise(PosState::Initial))
        );

        IntTreeStackAutomaton::new(t_auto, inter1, inter2)
    }
}
