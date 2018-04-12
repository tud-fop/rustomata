/// extraction of Dyck words from an FSA over bracket words similar as described by Hulden, 2011
/// TODO: * this should not need terminal-separated rules

use dyck::Bracket;
use grammars::lcfrs::cs_representation::{automata::Delta, automata::FiniteAutomaton,
                                         bracket_fragment::BracketFragment, BracketContent};
use integeriser::Integeriser;
use num_traits::{One, Zero};
use std::ops::Mul;
use util::{search::{Search, WeightedSearchItem},
           agenda::Capacity};

use std::{collections::HashMap, hash::Hash, iter::once};

#[derive(Debug, PartialEq, Eq)]
struct CykGeneratorHeuristic<W> {
    forward: HashMap<usize, W>,
    backward: HashMap<usize, W>,
}

impl<W> CykGeneratorHeuristic<W> {
    fn new<C: Hash + Eq>(fsa: &ExplodedAutomaton<C, W>) -> Self
    where
        W: One + Mul<Output = W> + Copy + Ord,
    {
        let forward = Search::weighted(
            fsa.finals.iter().map(|q| WeightedSearchItem(*q, W::one())),
            |&WeightedSearchItem(q, w)| {
                fsa.backward_state_transition
                    .get(&q)
                    .into_iter()
                    .flat_map(|v| v.iter().map(|&(q_, w_)| WeightedSearchItem(q_, w_ * w)))
                    .collect()
            },
        ).uniques()
            .map(|WeightedSearchItem(q, w)| (q, w))
            .collect();

        let backward = Search::weighted(
            vec![WeightedSearchItem(fsa.initial, W::one())],
            |&WeightedSearchItem(q, w)| {
                fsa.forward_state_transition
                    .get(&q)
                    .into_iter()
                    .flat_map(|v| v.iter().map(|&(q_, w_)| WeightedSearchItem(q_, w * w_)))
                    .collect()
            },
        ).uniques()
            .map(|WeightedSearchItem(q, w)| (q, w))
            .collect();

        CykGeneratorHeuristic { forward, backward }
    }

    fn wrap(&self, p: &usize, w: W, q: &usize) -> W
    where
        W: Zero + Mul<Output = W> + Copy,
    {
        self.backward.get(p).map(|p| *p).unwrap_or(W::zero()) * w
            * self.forward.get(q).map(|p| *p).unwrap_or(W::zero())
    }
}

#[derive(Debug)]
struct ExplodedAutomaton<T, W>
where
    T: Eq + Hash,
{
    initial: usize,
    finals: Vec<usize>,

    forward_state_transition: HashMap<usize, Vec<(usize, W)>>,
    backward_state_transition: HashMap<usize, Vec<(usize, W)>>,

    terminal: Vec<(usize, Vec<Bracket<T>>, W, usize)>,
    opening_brackets: Vec<(usize, T, W, usize)>,
    closing_brackets: HashMap<T, Vec<(usize, W, usize)>>,
}

impl<W, T> ExplodedAutomaton<BracketContent<T>, W>
where
    W: Copy + One,
    T: Hash + Eq + Clone,
{
    fn new(fsa: FiniteAutomaton<BracketFragment<T>, W>) -> Self {
        // contains (p', ⟨_t, p) such that (p', t, weight, p)
        let mut opening = Vec::new();
        // contains (q, ⟩_t, q') such that t -> (q, weight, q')
        let mut closing = HashMap::new();
        // forward state transitions without labels
        let mut forward = HashMap::new();
        // backward state transitions without labels
        let mut backward = HashMap::new();
        // all bracket fragments with terminal symbols
        let mut initial = Vec::new();

        // counter for unique usize states while exploding fsa transitions
        let mut uniquestate = fsa.arcs.len();

        for (from, tos) in fsa.arcs.into_iter().enumerate() {
            for (ilabel, (to, weight)) in tos.into_iter() {
                let BracketFragment(mut brackets) = fsa.labels.find_value(ilabel).unwrap().clone();
                if brackets.len() == 2 {
                    let bracket = brackets.remove(0);
                    match bracket {
                        Bracket::Open(cont) => {
                            opening.push((from, cont, weight, uniquestate));
                            forward
                                .entry(from)
                                .or_insert_with(Vec::new)
                                .push((uniquestate, weight));
                            backward
                                .entry(uniquestate)
                                .or_insert_with(Vec::new)
                                .push((from, weight));
                        }
                        Bracket::Close(cont) => {
                            closing.entry(cont).or_insert_with(Vec::new).push((
                                from,
                                W::one(),
                                uniquestate,
                            ));
                            forward
                                .entry(from)
                                .or_insert_with(Vec::new)
                                .push((uniquestate, W::one()));
                            backward
                                .entry(uniquestate)
                                .or_insert_with(Vec::new)
                                .push((from, W::one()));
                        }
                    }

                    let bracket = brackets.remove(0);
                    match bracket {
                        Bracket::Open(cont) => {
                            opening.push((uniquestate, cont, W::one(), to));
                            forward
                                .entry(uniquestate)
                                .or_insert_with(Vec::new)
                                .push((to, W::one()));
                            backward
                                .entry(to)
                                .or_insert_with(Vec::new)
                                .push((uniquestate, W::one()));
                        }
                        Bracket::Close(cont) => {
                            closing.entry(cont).or_insert_with(Vec::new).push((
                                uniquestate,
                                weight,
                                to,
                            ));
                            forward
                                .entry(uniquestate)
                                .or_insert_with(Vec::new)
                                .push((to, weight));
                            backward
                                .entry(to)
                                .or_insert_with(Vec::new)
                                .push((uniquestate, weight));
                        }
                    }

                    uniquestate += 1;
                } else if brackets.len() == 4 {
                    initial.push((from, brackets, weight, to));
                    forward
                        .entry(from)
                        .or_insert_with(Vec::new)
                        .push((to, weight));
                    backward
                        .entry(to)
                        .or_insert_with(Vec::new)
                        .push((from, weight));
                } else {
                    panic!("encountered `BracketFragment` with length other than 2 and 4");
                }
            }
        }

        ExplodedAutomaton {
            initial: fsa.initial,
            finals: fsa.finals,

            forward_state_transition: forward,
            backward_state_transition: backward,

            terminal: initial,
            opening_brackets: opening,
            closing_brackets: closing,
        }
    }
}

/// Implements the extraction of Dyck words from an FSA.
pub fn cyk_generator<T, W>(
    fsa: FiniteAutomaton<BracketFragment<T>, W>,
    beamwidth: Capacity
) -> impl Iterator<Item = Vec<Delta<T>>>
where
    T: Clone + Hash + Eq + Ord,
    W: Mul<Output = W> + Zero + One + Ord + Copy,
{
    let exploded = ExplodedAutomaton::new(fsa);
    let h = CykGeneratorHeuristic::new(&exploded);
    let ExplodedAutomaton {
        initial,
        finals,
        terminal,
        opening_brackets,
        closing_brackets,
        ..
    } = exploded;

    // store matching bracket pairs, with weight
    // (p, q) -> (p', t, w1, w2, q')
    let mut bpairs = HashMap::new();
    for (from1, cont, w1, to1) in opening_brackets {
        for &(from2, w2, to2) in closing_brackets.get(&cont).unwrap_or(&Vec::new()) {
            bpairs.entry((to1, from2)).or_insert_with(Vec::new).push((
                from1,
                cont.clone(),
                w1,
                w2,
                to2,
            ));
        }
    }

    let mut marked_from_left = HashMap::new();
    let mut marked_from_right = HashMap::new();

    Search::weighted(
        terminal
            .into_iter()
            .map(|(p, word, weight, q)| {
                WeightedSearchItem((p, word, weight, q), h.wrap(&p, weight, &q))
            }).collect::<Vec<_>>(),
        move |&WeightedSearchItem((from, ref word, weight, to), _)| {
            let mut unmarked = Vec::new();

            // mark word
            marked_from_left
                .entry(from)
                .or_insert_with(Vec::new)
                .push((word.clone(), weight, to));
            marked_from_right
                .entry(to)
                .or_insert_with(Vec::new)
                .push((from, word.clone(), weight));

            // insert all dyck words with fitting states from left and right to unmarked agenda
            for &(ref word_, weight_, to_) in marked_from_left.get(&to).unwrap_or(&Vec::new()) {
                let succweight = weight * weight_;
                let succprio = h.wrap(&from, succweight, &to_);
                if !succprio.is_zero() {
                    unmarked.push(WeightedSearchItem(
                        (
                            from,
                            word.iter().chain(word_.iter()).cloned().collect(),
                            succweight,
                            to_,
                        ),
                        succprio,
                    ));
                }
            }
            for &(from_, ref word_, weight_) in marked_from_right.get(&from).unwrap_or(&Vec::new())
            {
                let succweight = weight_ * weight;
                let succprio = h.wrap(&from_, succweight, &to);
                if !succprio.is_zero() {
                    unmarked.push(WeightedSearchItem(
                        (
                            from_,
                            word_.iter().chain(word.iter()).cloned().collect(),
                            succweight,
                            to,
                        ),
                        succprio,
                    ));
                }
            }

            // match bracket pairs around word
            for &(from_, ref bracket_content, wl, wr, to_) in
                bpairs.get(&(from, to)).unwrap_or(&Vec::new())
            {
                let succweight = wl * weight * wr;
                let succprio = h.wrap(&from_, succweight, &to_);
                if !succprio.is_zero() {
                    unmarked.push(WeightedSearchItem(
                        (
                            from_,
                            once(Bracket::Open(bracket_content.clone()))
                                .chain(word.iter().cloned())
                                .chain(once(Bracket::Close(bracket_content.clone())))
                                .collect(),
                            succweight,
                            to_,
                        ),
                        succprio,
                    ));
                }
            }

            unmarked
        },
    ).beam(beamwidth)
     .filter_map(move |WeightedSearchItem((p, w, _, q), _)| {
        if p == initial && finals.contains(&q) {
            Some(w)
        } else {
            None
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use grammars::lcfrs::cs_representation::{automata::finite_automaton::StateInstruction,
                                             BracketContent};
    use log_domain::LogDomain;
    use num_traits::One;
    use recognisable::Transition;
    use util::reverse::Reverse;

    #[test]
    fn test_explode() {
        let exploded = ExplodedAutomaton::new(example_fsa());
        let one = LogDomain::one().into();

        assert_eq!(exploded.initial, 0);
        assert_eq!(exploded.finals, vec![1]);
        assert_eq!(
            exploded.forward_state_transition,
            vec![
                (0, vec![(1, one), (2, one)]),
                (2, vec![(0, one)]),
                (1, vec![(3, one)]),
                (3, vec![(1, one)]),
            ].into_iter()
                .collect()
        );
        assert_eq!(
            exploded.backward_state_transition,
            vec![
                (0, vec![(2, one)]),
                (2, vec![(0, one)]),
                (1, vec![(0, one), (3, one)]),
                (3, vec![(1, one)]),
            ].into_iter()
                .collect(),
        );
        assert_eq!(
            exploded.terminal,
            vec![(
                0,
                vec![
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Component(1, 0)),
                ],
                one,
                1,
            )]
        );
        assert_eq!(
            exploded.opening_brackets,
            vec![
                (0, BracketContent::Component(0, 0), one, 2),
                (2, BracketContent::Variable(0, 0, 0), one, 0),
            ]
        );
        assert_eq!(
            exploded.closing_brackets,
            vec![
                (BracketContent::Variable(0, 0, 0), vec![(1, one, 3)]),
                (BracketContent::Component(0, 0), vec![(3, one, 1)]),
            ].into_iter()
                .collect()
        )
    }

    #[test]
    fn test_cyk_heuristic() {
        let h = CykGeneratorHeuristic::new(&ExplodedAutomaton::new(example_fsa()));
        assert_eq!(
            CykGeneratorHeuristic {
                forward: vec![0, 1, 2, 3]
                    .into_iter()
                    .map(|q| (q, LogDomain::new(1.0).unwrap().into()))
                    .collect(),
                backward: vec![0, 1, 2, 3]
                    .into_iter()
                    .map(|q| (q, LogDomain::new(1.0).unwrap().into()))
                    .collect(),
            },
            h
        );

        let v = LogDomain::new(0.5).unwrap().into();
        assert_eq!(v, h.wrap(&0, v, &1));
    }

    #[test]
    fn test_cykgen_iterator() {
        assert_eq!(
            cyk_generator(example_fsa(), Capacity::Infinite).take(2).collect::<Vec<_>>(),
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
            ]
        )
    }

    fn example_fsa() -> FiniteAutomaton<BracketFragment<String>, Reverse<LogDomain<f64>>> {
        let arcs = vec![
            Transition {
                weight: LogDomain::new(1.0).unwrap().into(),
                instruction: StateInstruction(0, 0),
                word: vec![BracketFragment(vec![
                    Bracket::Open(BracketContent::Component(0, 0)),
                    Bracket::Open(BracketContent::Variable(0, 0, 0)),
                ])],
            },
            Transition {
                weight: LogDomain::new(1.0).unwrap().into(),
                instruction: StateInstruction(0, 1),
                word: vec![BracketFragment(vec![
                    Bracket::Open(BracketContent::Component(1, 0)),
                    Bracket::Open(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Terminal("a".to_owned())),
                    Bracket::Close(BracketContent::Component(1, 0)),
                ])],
            },
            Transition {
                weight: LogDomain::new(1.0).unwrap().into(),
                instruction: StateInstruction(1, 1),
                word: vec![BracketFragment(vec![
                    Bracket::Close(BracketContent::Variable(0, 0, 0)),
                    Bracket::Close(BracketContent::Component(0, 0)),
                ])],
            },
        ];

        FiniteAutomaton::new(arcs, 0, vec![1])
    }
}
