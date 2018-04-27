use integeriser::{HashIntegeriser, Integeriser};
use grammars::pmcfg::PMCFGRule;
use std::hash::Hash;
use grammars::lcfrs::cs_representation::bracket_fragment::BracketFragment;

use super::{FiniteAutomaton, Generator, StateInstruction};

use std::collections::{HashMap, HashSet, BTreeSet};
use std::rc::Rc;

use recognisable::Transition;
use grammars::lcfrs::cs_representation::rule_fragments::fragments;
use util::{IntMap, factorizable::Factorizable};
use util::search::Search;
use std::ops::Mul;

pub enum FilterStrategy {
    Naive,
    Inside,
}


/// A `FilterAutomaton` is a structure that produces a `FiniteAutomaton`
/// with respect to a grammar w ∈ Σ and a word s.t.
/// it recognizes all bracket words δ ∈ Δ* that represent a
/// derivation of the word in the grammar h(δ) = w.
/// The generation of this `FiniteAutomaton` is divided into
/// two steps; an initialization and the generation itself.
#[derive(Debug, Serialize, Deserialize)]
pub enum Filter<T: Eq + Hash> {
    Naive {
        brackets_with: HashMap<T, Vec<(Vec<T>, usize)>>,
        epsilon_brackets: Vec<usize>,
    },
    Inside {
        free_nts: Vec<usize>,
        free_brackets: Vec<usize>,

        nt: IntMap<Vec<(Vec<usize>, Vec<usize>, usize)>>,
        nt_and_t: HashMap<T, Vec<(Vec<usize>, Vec<(Vec<T>, usize)>, usize)>>,
    },
}

impl<'a, T> Filter<T>
where
    T: Hash + Eq + Clone + 'a + ::std::fmt::Debug,
{
    pub fn naive<R, I, N, W>(grammar_rules: R, integeriser: &I, reference: &Generator<T, W>) -> Self
    where
        N: Hash + Eq + Clone + 'a,
        W: Eq + Copy + 'a + Mul<Output=W> + Factorizable + ::std::fmt::Debug,
        R: Iterator<Item = &'a PMCFGRule<N, T, W>>,
        I: Integeriser<Item = PMCFGRule<N, T, W>>,
    {
        let integerizer = reference.get_integeriser();
        let mut brackets_with = HashMap::new();
        let mut epsilon_brackets = Vec::new();

        for fragment in grammar_rules.flat_map(|r| fragments(r)) {
            let brackets = integerizer
                .find_key(&fragment.bracket_word(integeriser))
                .unwrap();

            if fragment.terminals().is_empty() {
                epsilon_brackets.push(brackets);
            } else {
                for symbol in fragment.terminals() {
                    brackets_with
                        .entry((*symbol).clone())
                        .or_insert(Vec::new())
                        .push((
                            fragment.terminals().iter().map(|s| (*s).clone()).collect(),
                            brackets,
                        ));
                }
            }
        }

        Filter::Naive {
            brackets_with,
            epsilon_brackets,
        }
    }

    pub fn inside<R, I, N, W>(grammar_rules: R, integeriser: &I, reference: &Generator<T, W>) -> Self
    where
        N: Hash + Eq + Clone + 'a,
        W: Eq + Copy + 'a + Mul<Output=W> + Factorizable + ::std::fmt::Debug,
        R: Iterator<Item = &'a PMCFGRule<N, T, W>>,
        I: Integeriser<Item = PMCFGRule<N, T, W>>,
    {
        let mut dependencies = HashIntegeriser::new();
        let bracket_integeriser = reference.get_integeriser();

        let mut free_nts = Vec::new();
        let mut free_brackets = Vec::new();

        let mut nt = IntMap::default();
        let mut nt_and_t = HashMap::new();

        for rule in grammar_rules {
            let frags: Vec<_> = fragments(rule).collect();
            let head = dependencies.integerise(rule.head.clone());

            let mut tss_brackets = Vec::new();
            for fragment in frags {
                let bw: usize = bracket_integeriser
                    .find_key(&fragment.bracket_word(integeriser))
                    .unwrap();
                let ts: Vec<T> = fragment.terminals().iter().map(|t| *t).cloned().collect();
                tss_brackets.push((ts, bw));
            }

            let deps: Vec<_> = rule.tail
                .iter()
                .map(|nt| dependencies.integerise(nt.clone()))
                .collect();

            if tss_brackets.iter().all(|&(ref ts, _)| ts.is_empty()) {
                if rule.tail.is_empty() {
                    free_nts.push(head);
                    free_brackets.extend(tss_brackets.into_iter().map(|(_, bw)| bw));
                } else {
                    let bws: Vec<_> = tss_brackets.into_iter().map(|(_, bw)| bw).collect();
                    for a in &deps {
                        nt.entry(*a).or_insert_with(Vec::new).push((
                            deps.clone(),
                            bws.clone(),
                            head,
                        ));
                    }
                }
            } else {
                let t = (*tss_brackets
                             .iter()
                             .flat_map(|&(ref ts, _)| (*ts).iter())
                             .next()
                             .unwrap())
                    .clone();
                nt_and_t.entry(t).or_insert_with(Vec::new).push((
                    deps.clone(),
                    tss_brackets
                        .clone(),
                    head,
                ));
            }
        }

        Filter::Inside {
            free_nts,
            free_brackets,
            nt,
            nt_and_t,
        }
    }

    /// Extracts an unweighted finite state automaton from the object.
    pub fn fsa<W>(
        &self,
        word: &[T],
        reference_automaton: &Generator<T, W>,
    ) -> FiniteAutomaton<BracketFragment<T>, ()> where W:  ::std::fmt::Debug {
        match *self {
            Filter::Naive {
                ref brackets_with,
                ref epsilon_brackets,
            } => {
                let mut arcs = Vec::new();
                for i in 0..(word.len()) {
                    for &(ref terminals, i_brackets) in
                        brackets_with.get(&word[i]).unwrap_or(&Vec::new())
                    {
                        if word[i..(i + terminals.len())] == terminals[..] {
                            arcs.push(Transition {
                                instruction: StateInstruction(i, i + terminals.len()),
                                word: vec![i_brackets],
                                weight: (),
                            });
                        }
                    }
                    arcs.extend(epsilon_brackets.iter().map(|brackets| {
                        Transition {
                            instruction: StateInstruction(i, i),
                            word: vec![*brackets],
                            weight: (),
                        }
                    }));
                }
                arcs.extend(epsilon_brackets.iter().map(|brackets| {
                    Transition {
                        instruction: StateInstruction(word.len(), word.len()),
                        word: vec![*brackets],
                        weight: (),
                    }
                }));
                FiniteAutomaton::from_integerized(
                    arcs,
                    0,
                    vec![word.len()],
                    Rc::clone(&reference_automaton.get_integeriser()),
                )
            }
            Filter::Inside {
                ref free_nts,
                ref free_brackets,
                ref nt,
                ref nt_and_t,
            } => {
                let mut arcs: Vec<Transition<StateInstruction<usize>, usize, ()>> = Vec::new();
                for b in free_brackets {
                    arcs.extend((0..word.len() + 1).map(|i| {
                        Transition {
                            instruction: StateInstruction(i, i),
                            word: vec![*b],
                            weight: (),
                        }
                    }));
                }
                let mut nts: BTreeSet<usize> = free_nts.iter().cloned().collect();

                let epsilons = nt.clone();
                let mut with_ts = IntMap::default();

                let symbols: HashSet<_> = word.iter().cloned().collect();
                for symbol in symbols {
                    for &(ref required_nts, ref ts_b_s, n) in
                        nt_and_t.get(&symbol).unwrap_or(&Vec::new())
                    {
                        let pos_bs: Vec<Vec<_>> = ts_b_s
                            .iter()
                            .map(|&(ref terms, bs)| if terms.is_empty() {
                                (0..word.len() + 1).map(|i| (i, bs, i)).collect()
                            } else {
                                word.windows(terms.len())
                                    .enumerate()
                                    .filter_map(|(i, window)| if window == terms.as_slice() {
                                        Some((i, bs, i + terms.len()))
                                    } else {
                                        None
                                    })
                                    .collect()
                            })
                            .collect();
                        if pos_bs.iter().all(|matches| !matches.is_empty()) {
                            if required_nts.is_empty() {
                                arcs.extend(pos_bs.into_iter().flat_map(|v| {
                                    v.into_iter().map(|(from, label, to)| {
                                        Transition {
                                            instruction: StateInstruction(from, to),
                                            word: vec![label],
                                            weight: (),
                                        }
                                    })
                                }));
                                nts.insert(n);
                            } else {
                                for nt in &nts {
                                    with_ts.entry(*nt).or_insert_with(Vec::new).push((
                                        nts.clone(),
                                        pos_bs.clone(),
                                        n,
                                    ));
                                }
                            }
                        }
                    }
                }

                let initials: Vec<_> = nts.iter().cloned().collect();
                for _ in Search::unweighted(initials, |a| {
                    let mut succ = Vec::new();
                    for &(ref requires, ref bs, ensures) in
                        epsilons.get(&a).unwrap_or(&Vec::new())
                    {
                        if requires.iter().all(|i| nts.contains(i)) {
                            nts.insert(ensures);
                            for q in 0..word.len() + 1 {
                                for b in bs {
                                    arcs.push(Transition {
                                        instruction: StateInstruction(q, q),
                                        word: vec![*b],
                                        weight: (),
                                    });
                                }
                            }
                            succ.push(ensures);
                        }
                    }
                    for &(ref requires, ref bs, ensures) in with_ts.get(&a).unwrap_or(&Vec::new()) {
                        if requires.iter().all(|i| nts.contains(i)) {
                            nts.insert(ensures);
                            arcs.extend(bs.iter().flat_map(|b| {
                                b.iter().map(|&(q, l, q_)| {
                                    Transition {
                                        instruction: StateInstruction(q, q_),
                                        word: vec![l],
                                        weight: (),
                                    }
                                })
                            }));
                            succ.push(ensures);
                        }
                    }
                    succ
                }).uniques()
                {}

                FiniteAutomaton::from_integerized(
                    arcs,
                    0,
                    vec![word.len()],
                    Rc::clone(&reference_automaton.get_integeriser()),
                )
            }
        }
    }
}
