extern crate log_domain;
extern crate num_traits;
extern crate rustomata;

use log_domain::LogDomain;
use std::fs::File;
use std::io::Read;
use std::marker::PhantomData;

use rustomata::approximation::ApproximationStrategy;
use rustomata::approximation::equivalence_classes::EquivalenceRelation;
use rustomata::approximation::relabel::RlbElement;
use rustomata::cfg::*;
use rustomata::push_down_automaton::*;
use rustomata::recognisable::*;

fn example_pushdown_automaton()
    -> PushDownAutomaton<PushState<String, String>, String, LogDomain<f64>>
{
    let mut grammar_file = File::open("examples/example2.cfg").unwrap();
    let mut grammar_string = String::new();
    let _ = grammar_file.read_to_string(&mut grammar_string);
    let grammar: CFG<String, String, _> = grammar_string.parse().unwrap();

    PushDownAutomaton::from(grammar)
}

fn example_equivalence_relation() -> EquivalenceRelation<String, String> {
    let mut relation_file = File::open("examples/example.classes").unwrap();
    let mut relation_string = String::new();
    let _ = relation_file.read_to_string(&mut relation_string);

    relation_string.parse().unwrap()
}

#[test]
fn test_relabel_pushdown_correctness() {
    let automaton = example_pushdown_automaton();
    let rel = example_equivalence_relation();
    let mapping = |ps: &PushState<_, _>| ps.map(|nt| rel.project(nt));
    let rlb = RlbElement::new(&mapping);
    let (relabelled_automaton, _) = rlb.approximate_automaton(&automaton);

    let true_positives_and_true_negatives = vec![
        "aab",
        "bba",
        "aaabb",
        "aabba",
        "",
        "aa",
        "aaab",
        "bbbbbb",
    ];

    for word in true_positives_and_true_negatives {
        let input: Vec<_> = String::from(word).chars().map(|x| x.to_string()).collect();
        assert_eq!(
            automaton.recognise(input.clone()).next().is_some(),
            relabelled_automaton.recognise(input).next().is_some()
        );
    }

    let false_positives = vec![
        "aaa",
        "bbb",
        "aabaa",
        "abaaa",
    ];

    for word in false_positives {
        let input: Vec<_> = String::from(word).chars().map(|x| x.to_string()).collect();
        assert_eq!(false, automaton.recognise(input.clone()).next().is_some());
        assert_eq!(true, relabelled_automaton.recognise(input).next().is_some());
    }
}

#[test]
fn test_cfg_from_str_correctness() {
    let rule_s0 = CFGRule {
        head: 'S',
        composition: CFGComposition { composition: vec![
            LetterT::Value('a'), LetterT::Label('S'), LetterT::Value('b'),
        ] },
        weight: LogDomain::new(0.4).unwrap()
    };
    let rule_s1 = CFGRule {
        head: 'S',
        composition: CFGComposition { composition: vec![]},
        weight: LogDomain::new(0.6).unwrap()
    };
    let control_grammar = CFG {
        _dummy: PhantomData,
        initial: vec!['S'],
        rules: vec![rule_s0, rule_s1]
    };

    let mut grammar_file = File::open("examples/example.cfg").unwrap();
    let mut grammar_string = String::new();
    let _ = grammar_file.read_to_string(&mut grammar_string);
    let grammar: CFG<char, char, LogDomain<f64>> = grammar_string.parse().unwrap();

    assert_eq!(
        control_grammar.clone(),
        grammar.clone()
    );

    let control_automaton = PushDownAutomaton::from(control_grammar);
    let automaton = PushDownAutomaton::from(grammar);
    let words = vec![
        "",
        "aabb",
        "abb",
        "aab",
    ];

    for word in words {
        let input: Vec<_> = String::from(word).chars().collect();
        assert_eq!(
            control_automaton.recognise(input.clone()).next().is_some(),
            automaton.recognise(input).next().is_some()
        );
    }
}
