extern crate log_domain;
#[macro_use]
extern crate rustomata;

use log_domain::LogDomain;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use rustomata::approximation::ApproximationStrategy;
use rustomata::approximation::equivalence_classes::EquivalenceRelation;
use rustomata::approximation::relabel::RlbElement;
use rustomata::approximation::tts::TTSElement;
use rustomata::pmcfg::*;
use rustomata::pmcfg::negra::to_negra;
use rustomata::recognisable::*;
use rustomata::recognisable::coarse_to_fine::CoarseToFineRecogniser;
use rustomata::tree_stack_automaton::*;

fn example_tree_stack_automaton()
        -> TreeStackAutomaton<PosState<PMCFGRule<String, String, LogDomain<f64>>>, String, LogDomain<f64>>
{
    let mut grammar_file = File::open("examples/example.mcfg").unwrap();
    let mut grammar_string = String::new();
    let _ = grammar_file.read_to_string(&mut grammar_string);
    let grammar: PMCFG<String, String, _> = grammar_string.parse().unwrap();

    TreeStackAutomaton::from(grammar)
}

#[test]
fn test_example_pmcfg_to_negra() {
    let automaton = example_tree_stack_automaton();
    let tree_stack = automaton.recognise(
        String::from("aabccd").chars().map(|x| x.to_string()).collect()
    ).next().unwrap().0;

    let syntax_tree = to_abstract_syntax_tree(tree_stack.storage.to_tree());
    let separated_syntax_tree = separate_terminal_rules(&syntax_tree);

    let negra_string = to_negra(&separated_syntax_tree, 0);
    let negra_control_string = String::from(
        "#BOS 0\n\
         a\ta\t--\t--\t1\n\
         a\ta\t--\t--\t2\n\
         b\tb\t--\t--\t3\n\
         c\tc\t--\t--\t1\n\
         c\tc\t--\t--\t2\n\
         d\td\t--\t--\t3\n\
         #1\tA\t--\t--\t4\n\
         #2\tA\t--\t--\t1\n\
         #3\tB\t--\t--\t4\n\
         #4\tS\t--\t--\t0\n\
         #EOS 0"
    );

    assert_eq!(negra_control_string, negra_string);
}

#[test]
fn test_from_str_automaton() {
    let i1: TreeStackInstruction<String> = TreeStackInstruction::Up {
        n: 1,
        current_val: "zwei".to_string(),
        old_val: "drei".to_string(),
        new_val: "vier".to_string(),
    };

    let i2: TreeStackInstruction<String> = TreeStackInstruction::Push {
        n: 1,
        current_val: "zwei".to_string(),
        new_val: "vier".to_string(),
    };

    let i3: TreeStackInstruction<String> = TreeStackInstruction::Down {
        current_val: "zwei".to_string(),
        old_val: "drei".to_string(),
        new_val: "vier".to_string(),
    };

    let i1_string = "Up 1 zwei drei vier";
    let i1_string2 = "   Up 1  zwei  drei   vier  ";
    let i2_string = "Push 1 zwei vier";
    let i3_string = "Down zwei drei vier";

    assert_eq!(Ok(i1.clone()), i1_string.parse());
    assert_eq!(Ok(i1.clone()), i1_string2.parse());
    assert_eq!(Ok(i2.clone()), i2_string.parse());
    assert_eq!(Ok(i3.clone()), i3_string.parse());

    let t1 = Transition {
        word: vec!["hello".to_string(), "world".to_string()],
        weight: LogDomain::new(0.5).unwrap(),
        instruction: i1,
    };

    let t2 = Transition {
        word: vec!["\"hello\\".to_string(), "world".to_string()],
        weight: LogDomain::new(0.5).unwrap(),
        instruction: i2,
    };

    let t3 = Transition {
        word: vec!["\"hello\\".to_string(), "world".to_string()],
        weight: LogDomain::new(0.5).unwrap(),
        instruction: i3,
    };

    let mut t1_string = String::from("Transition [\"hello\",\"world\"] (");
    t1_string.push_str(i1_string);
    t1_string.push_str(") #0.5");

    let mut t2_string = String::from("Transition [\"\\\"hello\\\\\",\"world\"] (");
    t2_string.push_str(i2_string);
    t2_string.push_str(") #0.5");

    let mut t3_string = String::from("Transition [\"\\\"hello\\\\\",\"world\"] (");
    t3_string.push_str(i3_string);
    t3_string.push_str(") #0.5");

    assert_eq!(Ok(t1.clone()), t1_string.parse());
    assert_eq!(Ok(t2.clone()), t2_string.parse());
    assert_eq!(Ok(t3.clone()), t3_string.parse());

    let automaton: TreeStackAutomaton<String, String, LogDomain<f64>> = TreeStackAutomaton::new(
        vec![t1.clone(), t2.clone(), t3.clone()],
        TreeStack::new("eins".to_string())
    );

    let mut automaton_string: String = String::from("initial: eins\n\n");
    automaton_string.push_str(t1_string.as_str());
    automaton_string.push_str("\n");
    automaton_string.push_str(t2_string.as_str());
    automaton_string.push_str("\n");
    automaton_string.push_str(t3_string.as_str());
    automaton_string.push_str("\n");

    let automaton_parse: TreeStackAutomaton<String, String, LogDomain<f64>> = automaton_string.parse().unwrap();

    let ts1: Vec<_> = automaton.list_transitions().collect();
    let ts2: Vec<_> = automaton_parse.list_transitions().collect();

    assert_eq!(ts1, ts2);

    assert_eq!(
        automaton.initial(),
        automaton_parse.initial()
    );
}

#[test]
fn test_coarse_to_fine_recogniser_correctness() {
    let automaton = example_tree_stack_automaton();
    let input: Vec<_> = String::from("aabccd").chars().map(|x| x.to_string()).collect();
    let control_tree_stack = automaton.recognise(input.clone()).next().unwrap().0;

    let tts = TTSElement::new();
    let e: EquivalenceRelation<String, String> = "0 [A, B]\n1 *".parse().unwrap();
    let f = |ps: &PosState<_>| ps
        .map(|r: &PMCFGRule<_, _, _>| r.map_nonterminals(|nt| e.project(nt)));
    let rlb = RlbElement::new(&f);

    let recogniser = coarse_to_fine_recogniser!(automaton; tts, rlb);
    let coarse_to_fine_tree_stack = recogniser.recognise(input.clone()).next().unwrap().0;

    assert_eq!(control_tree_stack, coarse_to_fine_tree_stack);
}

#[test]
fn test_tts() {
    //create (and test) initial push down automata
    let r0_string = "S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B)   # 1";
    let r1_string = "A → [[T a, Var 0 0, T e],  [T c, Var 0 1]] (A   )   # 0.5";
    let r2_string = "A → [[],  []                             ] (    )   # 0.5";
    let r3_string = "B → [[T b, Var 0 0],  [T d, Var 0 1]     ] (B   )   # 0.5";
    let r4_string = "B → [[],  []                             ] (    )   # 0.5";

    let mut g_string = String::from("initial: [S]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r3_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r4_string.clone());

    let g: PMCFG<String, String, LogDomain<f64>> = g_string.parse().unwrap();

    let a = TreeStackAutomaton::from(g);

    let tts = TTSElement::new();

    let (b, _) = tts.approximate_automaton(&a);

    assert_ne!(None, a.recognise(vec!["a".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_eq!(None, a.recognise(vec!["a".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "e".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_eq!(None, b.recognise(vec!["a".to_string(), "e".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "c".to_string(), "d".to_string() ]).next());
}

#[test]
fn test_from_str_pmcfg() {
    let c0: Composition<String> = Composition {
        composition: vec![vec![VarT::Var(0, 0), VarT::Var(1, 0), VarT::Var(0, 1), VarT::Var(1, 1)]],
    };

    let c1: Composition<String> = Composition { composition: vec![vec![], vec![]] };

    let c2 = Composition {
        composition: vec![vec![VarT::T("a".to_string()),
                               VarT::Var(0, 0)],
                          vec![VarT::T("c".to_string()),
                               VarT::Var(0, 1)]],
    };

    let c3 = Composition {
        composition: vec![vec![VarT::T("b".to_string()),
                               VarT::Var(0, 0)],
                          vec![VarT::T("d".to_string()),
                               VarT::Var(0, 1)]],
    };

    let r0: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "S".to_string(),
        tail: vec!["A".to_string(), "B".to_string()],
        composition: c0.clone(),
        weight: LogDomain::new(1.0).unwrap(),
    };

    let r1: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "A".to_string(),
        tail: Vec::new(),
        composition: c1.clone(),
        weight: LogDomain::new(0.6).unwrap(),
    };

    let r2: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "A".to_string(),
        tail: vec!["A".to_string()],
        composition: c2.clone(),
        weight: LogDomain::new(0.4).unwrap(),
    };

    let r3: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "B".to_string(),
        tail: Vec::new(),
        composition: c1.clone(),
        weight: LogDomain::new(0.7).unwrap(),
    };

    let r4: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "B".to_string(),
        tail: vec!["B".to_string()],
        composition: c3.clone(),
        weight: LogDomain::new(0.3).unwrap(),
    };

    let r0_string = "\"S\" → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (\"A\", B)";
    let r1_string = "A → [[],[]] ()  # 0.6 % this is a comment";
    let r2_string = "A → [[T a, Var 0 0],[T c, Var 0 1]] (A)  # 0.4";
    let r3_string = "B → [[],[]] ()  # 0.7";
    let r4_string = "B → [[T b, Var 0 0],[T d, Var 0 1]] (B)  # 0.3";

    assert_eq!(Ok(r0.clone()),
               r0_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r1.clone()),
               r1_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r2.clone()),
               r2_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r3.clone()),
               r3_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r4.clone()),
               r4_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());

    let g: PMCFG<String, String, LogDomain<f64>> = PMCFG {
        initial: vec!["S".to_string()],
        rules: vec![r0.clone(), r1.clone(), r2.clone(), r3.clone(), r4.clone()],
    };

    let mut g_string = String::from("initial: [S]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r3_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r4_string.clone());

    assert_eq!(Ok(g.clone()), g_string.parse());

    let a = TreeStackAutomaton::from(g);

    assert_ne!(None, a.recognise(vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()]).next());
}
