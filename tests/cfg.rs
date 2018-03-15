extern crate log_domain;
extern crate num_traits;
extern crate rustomata;

use log_domain::LogDomain;
use num_traits::identities::One;
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
fn test_from_str_cfg() {
    let c0 : CFGComposition<String,String> = CFGComposition {
        composition: vec![LetterT::Label("A".to_string())],
    };

    let c1 : CFGComposition<String,String>  = CFGComposition {
        composition: vec![LetterT::Value("a".to_string()),LetterT::Label("A".to_string()),LetterT::Label("B".to_string())]
    };

    let c2 : CFGComposition<String,String>  = CFGComposition {
        composition: vec![LetterT::Value("a".to_string())],
    };

    let c3 : CFGComposition<String,String>  = CFGComposition {
        composition: vec![LetterT::Value("b".to_string())],
    };

    let r0: CFGRule<String, String, LogDomain<f64>> = CFGRule {
        head: "S".to_string(),
        composition: c0.clone(),
        weight: LogDomain::one(),
    };

    let r1: CFGRule<String, String, LogDomain<f64>> = CFGRule {
        head: "A".to_string(),
        composition: c1.clone(),
        weight: LogDomain::new(0.6).unwrap(),
    };

    let r2: CFGRule<String, String, LogDomain<f64>> = CFGRule {
        head: "A".to_string(),
        composition: c2.clone(),
        weight: LogDomain::new(0.4).unwrap(),
    };

    let r3: CFGRule<String, String, LogDomain<f64>> = CFGRule {
        head: "B".to_string(),
        composition: c3.clone(),
        weight: LogDomain::one(),
    };

    let r0_string = "S → [Nt A]";
    let r1_string = "A → [T a, Nt A, Nt B] # 0.6";
    let r2_string = "A → [T a] # 0.4";
    let r3_string = "B → [T b] # 1";

    assert_eq!(Ok(r0.clone()),
               r0_string.parse::<CFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r1.clone()),
               r1_string.parse::<CFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r2.clone()),
               r2_string.parse::<CFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r3.clone()),
               r3_string.parse::<CFGRule<String, String, LogDomain<f64>>>());

    let g: CFG<String, String, LogDomain<f64>> = CFG {
        _dummy: PhantomData,
        initial: vec!["S".to_string(),"B".to_string()],
        rules: vec![r0.clone(), r1.clone(), r2.clone(), r3.clone()],
    };

    let mut g_string = String::from("initial: [S, B]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r3_string.clone());

    assert_eq!(Ok(g.clone()), g_string.parse());

    let a = PushDownAutomaton::from(g);

    assert_ne!(None, a.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string()]).next());
}
