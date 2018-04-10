/// extraction of Dyck words from an FSA over bracket words similar as described by Hulden, 2011
/// TODO: * port iterator to `Search`
///       * weighted
///       * heuristic using shorted paths - from state to final
///                                       - from initial to state

use dyck::Bracket;
use grammars::lcfrs::cs_representation::BracketContent;
use grammars::lcfrs::cs_representation::bracket_fragment::BracketFragment;
use grammars::lcfrs::cs_representation::automata::FiniteAutomaton;

use integeriser::Integeriser;

use std::collections::HashMap;
use std::hash::Hash;
use std::iter::once;

/// Implements the extraction of Dyck words from an FSA.
#[derive(Debug, PartialEq)]
pub struct CykGenerator<T>
where
    T: PartialEq,
{
    // initial and final states
    initial: usize,
    finals: Vec<usize>,

    // agenda of unmared bracket words
    unmarked: Vec<(usize, Vec<Bracket<T>>, usize)>,
    // marked word v spanned by states p, q such that p -> v, q
    marked_from_left: HashMap<usize, Vec<(Vec<Bracket<T>>, usize)>>,
    // marked word v spanned by states p, q such that q -> p, v
    marked_from_right: HashMap<usize, Vec<(usize, Vec<Bracket<T>>)>>,

    // contains transitions (p', ⟨_t, p), (q, ⟩_t, q') such that (p, q) -> (p', t, q')
    bpairs: HashMap<(usize, usize), Vec<(usize, T, usize)>>,
}

impl<T> CykGenerator<BracketContent<T>>
where
    T: Clone + Hash + Eq,
{
    pub fn new<W>(fsa: FiniteAutomaton<BracketFragment<T>, W>) -> Self {
        let FiniteAutomaton {
            initial,
            arcs,
            finals,
            labels,
        } = fsa;

        // counter for unique usize states while exploding fsa transitions
        let mut uniquestate = arcs.len();
        let mut inits = Vec::new();
        // contains (p', ⟨_t, p) such that (p', t, p)
        let mut opening = Vec::new();
        // contains (q, ⟩_t, q') such that t -> (q, q')
        let mut closing = HashMap::new();

        for (from, tos) in arcs.into_iter().enumerate() {
            for (ilabel, (to, _)) in tos.into_iter() {
                let BracketFragment(mut brackets) = labels.find_value(ilabel).unwrap().clone();
                if brackets.len() == 2 {

                    let bracket = brackets.remove(0);
                    match bracket {
                        Bracket::Open(cont) => {
                            opening.push((from, cont, uniquestate));
                        }
                        Bracket::Close(cont) => {
                            closing.entry(cont).or_insert_with(Vec::new).push((
                                from,
                                uniquestate,
                            ));
                        }
                    }

                    let bracket = brackets.remove(0);
                    match bracket {
                        Bracket::Open(cont) => {
                            opening.push((uniquestate, cont, to));
                        }
                        Bracket::Close(cont) => {
                            closing.entry(cont).or_insert_with(Vec::new).push((
                                uniquestate,
                                to,
                            ));
                        }
                    }

                    uniquestate += 1;

                } else if brackets.len() == 4 {
                    inits.push((from, brackets, to))
                } else {
                    panic!("encountered `BracketFragment` with length other than 2 and 4")
                }
            }
        }

        // store matching bracket pairs
        let mut bpairs = HashMap::new();
        for (from1, cont, to1) in opening {
            for &(from2, to2) in closing.get(&cont).unwrap_or(&Vec::new()) {
                bpairs.entry((to1, from2)).or_insert_with(Vec::new).push((
                    from1,
                    cont.clone(),
                    to2,
                ));
            }
        }

        CykGenerator {
            initial,
            finals,
            bpairs,

            unmarked: inits,

            marked_from_left: HashMap::new(),
            marked_from_right: HashMap::new(),
        }
    }
}

impl<T> Iterator for CykGenerator<T>
where
    T: Clone + PartialEq,
{
    type Item = Vec<Bracket<T>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.unmarked.len() > 0 {
            let (from, word, to) = self.unmarked.remove(0);

            // mark word
            self.marked_from_left
                .entry(from)
                .or_insert_with(Vec::new)
                .push((word.clone(), to));
            self.marked_from_right
                .entry(to)
                .or_insert_with(Vec::new)
                .push((from, word.clone()));

            // insert all dyck words with fitting states from left and right to unmarked agenda
            for &(ref word_, to_) in self.marked_from_left.get(&to).unwrap_or(&Vec::new()) {
                self.unmarked.push((
                    from,
                    word.iter().chain(word_.iter()).cloned().collect(),
                    to_,
                ));
            }
            for &(from_, ref word_) in self.marked_from_right.get(&from).unwrap_or(&Vec::new()) {
                self.unmarked.push((
                    from_,
                    word_.iter().chain(word.iter()).cloned().collect(),
                    to,
                ));
            }

            // match bracket pairs around word
            for &(from_, ref bracket_content, to_) in
                self.bpairs.get(&(from, to)).unwrap_or(&Vec::new())
            {
                self.unmarked.push((
                    from_,
                    once(Bracket::Open(bracket_content.clone()))
                        .chain(word.iter().cloned())
                        .chain(once(Bracket::Close(bracket_content.clone())))
                        .collect(),
                    to_,
                ));
            }

            if from == self.initial && self.finals.contains(&to) {
                return Some(word);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use log_domain::LogDomain;
    use recognisable::Transition;
    use grammars::lcfrs::cs_representation::automata::finite_automaton::StateInstruction;

    #[test]
    fn test_cykgen_construction() {
        let should_be = CykGenerator {
            initial: 0,
            finals: vec![1],

            unmarked: vec![
                (
                    0,
                    vec![
                        Bracket::Open(BracketContent::Component(1, 0)),
                        Bracket::Open(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Component(1, 0)),
                    ],
                    1
                ),
            ],
            marked_from_left: HashMap::new(),
            marked_from_right: HashMap::new(),

            bpairs: vec![
                ((2, 3), vec![(0, BracketContent::Component(0, 0), 1)]),
                ((0, 1), vec![(2, BracketContent::Variable(0, 0, 0), 3)]),
            ].into_iter()
                .collect(),
        };

        assert_eq!(CykGenerator::new(example_fsa()), should_be)
    }

    #[test]
    fn test_cykgen_iterator() {
        assert_eq!(
            CykGenerator::new(example_fsa()).take(2).collect::<Vec<_>>(),
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

    fn example_fsa() -> FiniteAutomaton<BracketFragment<String>, LogDomain<f64>> {
        let arcs = vec![
            Transition {
                weight: LogDomain::new(1.0).unwrap(),
                instruction: StateInstruction(0, 0),
                word: vec![
                    BracketFragment(vec![
                        Bracket::Open(BracketContent::Component(0, 0)),
                        Bracket::Open(BracketContent::Variable(0, 0, 0)),
                    ]),
                ],
            },
            Transition {
                weight: LogDomain::new(1.0).unwrap(),
                instruction: StateInstruction(0, 1),
                word: vec![
                    BracketFragment(vec![
                        Bracket::Open(BracketContent::Component(1, 0)),
                        Bracket::Open(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Terminal("a".to_owned())),
                        Bracket::Close(BracketContent::Component(1, 0)),
                    ]),
                ],
            },
            Transition {
                weight: LogDomain::new(1.0).unwrap(),
                instruction: StateInstruction(1, 1),
                word: vec![
                    BracketFragment(vec![
                        Bracket::Close(BracketContent::Variable(0, 0, 0)),
                        Bracket::Close(BracketContent::Component(0, 0)),
                    ]),
                ],
            },
        ];

        FiniteAutomaton::new(arcs, 0, vec![1])
    }
}
