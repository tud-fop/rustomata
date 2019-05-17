extern crate log_domain;
#[macro_use]
extern crate rustomata;

use log_domain::LogDomain;
use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use rustomata::approximation::ApproximationStrategy;
use rustomata::approximation::equivalence_classes::EquivalenceRelation;
use rustomata::approximation::relabel::RlbElement;
use rustomata::approximation::tts::TTSElement;
use rustomata::grammars::pmcfg::*;
use rustomata::grammars::pmcfg::negra::{to_negra, DumpMode};
use rustomata::recognisable::*;
use rustomata::recognisable::coarse_to_fine::CoarseToFineRecogniser;
use rustomata::automata::tree_stack_automaton::*;

fn pmcfg_from_file(grammar_file_path: &str) -> PMCFG<String, String, LogDomain<f64>> {
    let mut grammar_file = File::open(grammar_file_path).unwrap();
    let mut grammar_string = String::new();
    let _ = grammar_file.read_to_string(&mut grammar_string);
    grammar_string.parse().unwrap()
}

#[test]
fn test_example_pmcfg_to_negra() {
    let automaton = TreeStackAutomaton::from(pmcfg_from_file("examples/example.pmcfg"));
    let tree_stack = automaton
        .recognise(
            String::from("aabccd")
                .chars()
                .map(|x| x.to_string())
                .collect(),
        )
        .next()
        .unwrap()
        .0;

    let syntax_tree = to_abstract_syntax_tree(tree_stack.storage.to_tree());
    let separated_syntax_tree = separate_terminal_rules(&syntax_tree);

    let negra_string = to_negra(&separated_syntax_tree, 0, DumpMode::Default);
    let negra_control_string = String::from(
        "#BOS 0\n\
         a\ta\t--\t--\t500\n\
         a\ta\t--\t--\t501\n\
         b\tb\t--\t--\t502\n\
         c\tc\t--\t--\t500\n\
         c\tc\t--\t--\t501\n\
         d\td\t--\t--\t502\n\
         #500\tA\t--\t--\t0\n\
         #501\tA\t--\t--\t500\n\
         #502\tB\t--\t--\t0\n\
         #EOS 0",
    );

    assert_eq!(negra_control_string, negra_string);
}

#[test]
fn test_coarse_to_fine_recogniser_correctness() {
    let automaton = TreeStackAutomaton::from(pmcfg_from_file("examples/example.pmcfg"));
    let tts = TTSElement::new();
    let rel: EquivalenceRelation<String, String> = "0 [A, B]\n1 *".parse().unwrap();
    let mapping = |ps: &PosState<_>| {
        ps.map(|r: &PMCFGRule<_, _, _>| {
            r.map_nonterminals(|nt| rel.project(nt))
        })
    };
    let rlb = RlbElement::new(&mapping);
    let recogniser = coarse_to_fine_recogniser!(automaton.clone(); tts, rlb);

    let inputs = vec!["aabccd", "aaabcccd", "abccd", "abbcd"];

    for input in inputs {
        let word: Vec<_> = String::from(input).chars().map(|x| x.to_string()).collect();
        assert_eq!(
            automaton.recognise(word.clone()).next(),
            recogniser.recognise(word).next()
        );
    }
}

#[test]
fn test_tts_correctness() {
    let automaton = TreeStackAutomaton::from(pmcfg_from_file("examples/example.pmcfg"));
    let tts = TTSElement::new();
    let (tts_ed_automaton, _) = tts.approximate_automaton(&automaton);

    let true_positives_and_true_negatives = vec!["", "abcd", "aabccd", "aaabcccd"];

    for input in true_positives_and_true_negatives {
        let word: Vec<_> = String::from(input).chars().map(|x| x.to_string()).collect();
        assert_eq!(
            automaton.recognise(word.clone()).next().is_some(),
            tts_ed_automaton.recognise(word).next().is_some()
        );
    }

    let false_positives = vec!["aabcd", "aabbcd", "abbcccdddd", "abbbccdddd"];

    for input in false_positives {
        let word: Vec<_> = String::from(input).chars().map(|x| x.to_string()).collect();
        assert_eq!(false, automaton.recognise(word.clone()).next().is_some());
        assert_eq!(true, tts_ed_automaton.recognise(word).next().is_some());
    }
}

#[test]
fn test_pmcfg_from_str_correctness() {
    let rule_s0 = PMCFGRule {
        head: String::from("S"),
        tail: vec![String::from("A"), String::from("B")],
        composition: Composition {
            composition: vec![
                vec![
                    VarT::Var(0, 0),
                    VarT::Var(1, 0),
                    VarT::Var(0, 1),
                    VarT::Var(1, 1),
                ],
            ],
        },
        weight: LogDomain::new(1.0).unwrap(),
    };
    let rule_a0 = PMCFGRule {
        head: String::from("A"),
        tail: vec![String::from("A")],
        composition: Composition {
            composition: vec![
                vec![VarT::T(String::from("a")), VarT::Var(0, 0)],
                vec![VarT::T(String::from("c")), VarT::Var(0, 1)],
            ],
        },
        weight: LogDomain::new(0.5).unwrap(),
    };
    let rule_a1 = PMCFGRule {
        head: String::from("A"),
        tail: vec![],
        composition: Composition { composition: vec![vec![], vec![]] },
        weight: LogDomain::new(0.5).unwrap(),
    };
    let rule_b0 = PMCFGRule {
        head: String::from("B"),
        tail: vec![String::from("B")],
        composition: Composition {
            composition: vec![
                vec![VarT::T(String::from("b")), VarT::Var(0, 0)],
                vec![VarT::T(String::from("d")), VarT::Var(0, 1)],
            ],
        },
        weight: LogDomain::new(0.5).unwrap(),
    };
    let rule_b1 = PMCFGRule {
        head: String::from("B"),
        tail: vec![],
        composition: Composition { composition: vec![vec![], vec![]] },
        weight: LogDomain::new(0.5).unwrap(),
    };
    let control_grammar = PMCFG {
        initial: vec![String::from("S")],
        rules: vec![rule_s0, rule_a0, rule_a1, rule_b0, rule_b1],
    };

    let grammar = pmcfg_from_file("examples/example.pmcfg");
    assert_eq!(control_grammar.clone(), grammar.clone());

    let control_automaton = TreeStackAutomaton::from(control_grammar);
    let automaton = TreeStackAutomaton::from(grammar);
    let inputs = vec!["", "aabccd", "aabbcd", "aabccdd"];

    for input in inputs {
        let word: Vec<_> = String::from(input).chars().map(|x| x.to_string()).collect();
        assert_eq!(
            control_automaton.recognise(word.clone()).next().is_some(),
            automaton.recognise(word).next().is_some()
        );
    }
}

#[test]
fn test_tree_stack_automaton_from_str() {
    use crate::TreeStackInstruction::{Up, Down, Push};

    let unprocessed_transitions = vec![
        (
            "",
            Push {
                n: 0,
                current_val: 1.to_string(),
                new_val: 2.to_string(),
            },
            1.0
        ),
        (
            "",
            Push {
                n: 0,
                current_val: 1.to_string(),
                new_val: 3.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 4.to_string(),
                old_val: 18.to_string(),
                new_val: 19.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 5.to_string(),
                old_val: 6.to_string(),
                new_val: 5.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 5.to_string(),
                old_val: 7.to_string(),
                new_val: 8.to_string(),
            },
            1.0
        ),
        (
            "d",
            Up {
                n: 0,
                current_val: 9.to_string(),
                old_val: 10.to_string(),
                new_val: 11.to_string(),
            },
            1.0
        ),
        (
            "d",
            Up {
                n: 0,
                current_val: 9.to_string(),
                old_val: 5.to_string(),
                new_val: 9.to_string(),
            },
            1.0
        ),
        (
            "",
            Up {
                n: 0,
                current_val: 12.to_string(),
                old_val: 2.to_string(),
                new_val: 13.to_string(),
            },
            1.0
        ),
        (
            "",
            Up {
                n: 0,
                current_val: 12.to_string(),
                old_val: 14.to_string(),
                new_val: 15.to_string(),
            },
            1.0
        ),
        (
            "c",
            Up {
                n: 0,
                current_val: 6.to_string(),
                old_val: 13.to_string(),
                new_val: 10.to_string(),
            },
            1.0
        ),
        (
            "c",
            Up {
                n: 0,
                current_val: 6.to_string(),
                old_val: 16.to_string(),
                new_val: 6.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 11.to_string(),
                old_val: 9.to_string(),
                new_val: 17.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 11.to_string(),
                old_val: 8.to_string(),
                new_val: 4.to_string(),
            },
            1.0
        ),
        (
            "b",
            Up {
                n: 0,
                current_val: 15.to_string(),
                old_val: 2.to_string(),
                new_val: 13.to_string(),
            },
            1.0
        ),
        (
            "b",
            Up {
                n: 0,
                current_val: 15.to_string(),
                old_val: 14.to_string(),
                new_val: 15.to_string(),
            },
            1.0
        ),
        (
            "",
            Push {
                n: 0,
                current_val: 18.to_string(),
                new_val: 1.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 17.to_string(),
                old_val: 9.to_string(),
                new_val: 17.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 17.to_string(),
                old_val: 8.to_string(),
                new_val: 4.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 2.to_string(),
                old_val: 3.to_string(),
                new_val: 14.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 2.to_string(),
                old_val: 1.to_string(),
                new_val: 12.to_string(),
            },
            1.0
        ),
        (
            "",
            Up {
                n: 0,
                current_val: 8.to_string(),
                old_val: 10.to_string(),
                new_val: 11.to_string(),
            },
            1.0
        ),
        (
            "",
            Up {
                n: 0,
                current_val: 8.to_string(),
                old_val: 5.to_string(),
                new_val: 9.to_string(),
            },
            1.0
        ),
        (
            "a",
            Push {
                n: 0,
                current_val: 3.to_string(),
                new_val: 2.to_string(),
            },
            1.0
        ),
        (
            "a",
            Push {
                n: 0,
                current_val: 3.to_string(),
                new_val: 3.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 13.to_string(),
                old_val: 15.to_string(),
                new_val: 16.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 13.to_string(),
                old_val: 12.to_string(),
                new_val: 7.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 10.to_string(),
                old_val: 6.to_string(),
                new_val: 5.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 10.to_string(),
                old_val: 7.to_string(),
                new_val: 8.to_string(),
            },
            1.0
        ),
        (
            "",
            Up {
                n: 0,
                current_val: 7.to_string(),
                old_val: 13.to_string(),
                new_val: 10.to_string(),
            },
            1.0
        ),
        (
            "",
            Up {
                n: 0,
                current_val: 7.to_string(),
                old_val: 16.to_string(),
                new_val: 6.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 14.to_string(),
                old_val: 3.to_string(),
                new_val: 14.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 14.to_string(),
                old_val: 1.to_string(),
                new_val: 12.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 16.to_string(),
                old_val: 15.to_string(),
                new_val: 16.to_string(),
            },
            1.0
        ),
        (
            "",
            Down {
                current_val: 16.to_string(),
                old_val: 12.to_string(),
                new_val: 7.to_string(),
            },
            1.0
        ),
    ];

    let mut transitions = Vec::new();
    for (terminal, instruction, weight) in unprocessed_transitions {
        transitions.push(Transition {
            word: terminal.chars().map(|x| x.to_string()).collect(),
            instruction,
            weight: LogDomain::new(weight).unwrap(),
        });
    }
    let control_automaton = TreeStackAutomaton::new(transitions, TreeStack::new(18.to_string()));

    let mut automaton_file = File::open("examples/example.tsa").unwrap();
    let mut automaton_string = String::new();
    let _ = automaton_file.read_to_string(&mut automaton_string);
    let automaton: TreeStackAutomaton<String, String, LogDomain<f64>> =
        automaton_string.parse().unwrap();

    assert_eq!(control_automaton.initial(), automaton.initial());
    assert_eq!(
        control_automaton.list_transitions().collect::<HashSet<_>>(),
        automaton.list_transitions().collect::<HashSet<_>>()
    );
}

#[test]
fn test_pmcfg_recognise_legal_terminal_symbols() {
    let automaton = TreeStackAutomaton::from(pmcfg_from_file("examples/example.pmcfg"));
    let legal_inputs = vec![
        ("", true),
        ("aabccd", true),
        ("aabccdd", false),
        ("abc", false),
    ];

    for (legal_input, control_acceptance) in legal_inputs {
        let legal_word: Vec<_> = String::from(legal_input)
            .chars()
            .map(|x| x.to_string())
            .collect();
        assert_eq!(
            control_acceptance,
            automaton.recognise(legal_word).next().is_some()
        );
    }
}

#[test]
fn test_pmcfg_recognise_illegal_terminal_symbols() {
    let automaton = TreeStackAutomaton::from(pmcfg_from_file("examples/example.pmcfg"));
    let illegal_inputs = vec!["abce"];

    for ilegal_input in illegal_inputs {
        let illegal_word: Vec<_> = String::from(ilegal_input)
            .chars()
            .map(|x| x.to_string())
            .collect();
        assert!(automaton.recognise(illegal_word).next().is_none());
    }
}
