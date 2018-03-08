extern crate log_domain;
extern crate num_traits;
extern crate rustomata;

use log_domain::LogDomain;
use num_traits::identities::One;
use std::marker::PhantomData;

use rustomata::approximation::ApproximationStrategy;
use rustomata::approximation::equivalence_classes::EquivalenceRelation;
use rustomata::approximation::relabel::RlbElement;
use rustomata::cfg::*;
use rustomata::push_down_automaton::*;
use rustomata::recognisable::*;

#[test]
fn test_relabel_pushdown() {
    //create (and test) initial push down automata
    let r0_string = "S → [Nt A] # 1";
    let r1_string = "A → [T a, Nt A, Nt B  ] # 0.6";
    let r2_string = "A → [T a              ] # 0.4";
    let r3_string = "B → [T b, Nt B, Nt A  ] # 0.3";
    let r4_string = "B → [T b              ] # 0.7";

    let mut g_string = String::from("initial: [S, B]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r3_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r4_string.clone());

    let g: CFG<String, String, LogDomain<f64>> = g_string.parse().unwrap();

    let a = PushDownAutomaton::from(g);

    let mut e_string = String::from("S [S]\n");
    e_string.push_str("N [A, B]\n");
    e_string.push_str("R *\n");

    let e: EquivalenceRelation<String, String> = e_string.parse().unwrap();

    let f = |ps: &PushState<_, _>| ps.map(|nt| e.project(nt));
    let rlb = RlbElement::new(&f);

    let (b, _) = rlb.approximate_automaton(&a);

    assert_ne!(None, b.recognise(vec!["a".to_string() ]).next());
    assert_eq!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string(), "b".to_string(), "a".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string() ]).next());

    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]).next());
}

#[test]
fn test_relabel_check() {
    //create (and test) initial push down automata
    let r0_string = "S → [Nt A] # 1";
    let r1_string = "A → [T a, Nt A, Nt B  ] # 0.6";
    let r2_string = "A → [T a              ] # 0.4";
    let r3_string = "B → [T b, Nt B, Nt A  ] # 0.3";
    let r4_string = "B → [T b              ] # 0.7";

    let mut g_string = String::from("initial: [S, B]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r3_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r4_string.clone());

    let g: CFG<String, String, LogDomain<f64>> = g_string.parse().unwrap();

    let a = PushDownAutomaton::from(g);

    let mut e_string = String::from("S [S]\n");
    e_string.push_str("N [A, B]\n");
    e_string.push_str("R *\n");

    let e: EquivalenceRelation<String, String> = e_string.parse().unwrap();

    let f = |ps: &PushState<_, _>| ps.map(|nt| e.project(nt));
    let rlb = RlbElement::new(&f);

    let (b, _) = rlb.approximate_automaton(&a);

    let itemb = b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]).next();
    assert_ne!(None, itemb);
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
