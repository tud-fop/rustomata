extern crate log_domain;
extern crate rustomata;

use log_domain::LogDomain;

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
    e_string.push_str("R [*]\n");

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
