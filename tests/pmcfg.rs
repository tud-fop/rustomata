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

fn pmcfg_from_file(grammar_file_path: &str) -> PMCFG<String, String, LogDomain<f64>>
{
    let mut grammar_file = File::open(grammar_file_path).unwrap();
    let mut grammar_string = String::new();
    let _ = grammar_file.read_to_string(&mut grammar_string);
    grammar_string.parse().unwrap()
}

#[test]
fn test_example_pmcfg_to_negra() {
    let automaton = TreeStackAutomaton::from(pmcfg_from_file("examples/example.pmcfg"));
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
    let automaton = TreeStackAutomaton::from(pmcfg_from_file("examples/example.pmcfg"));
    let tts = TTSElement::new();
    let rel: EquivalenceRelation<String, String> = "0 [A, B]\n1 *".parse().unwrap();
    let mapping = |ps: &PosState<_>| ps
        .map(|r: &PMCFGRule<_, _, _>| r.map_nonterminals(|nt| rel.project(nt)));
    let rlb = RlbElement::new(&mapping);
    let recogniser = coarse_to_fine_recogniser!(automaton.clone(); tts, rlb);

    let words = vec![
        "aabccd",
        "aaabcccd",
        "abccd",
        "abbcd",
    ];

    for word in words {
        let input: Vec<_> = String::from(word).chars().map(|x| x.to_string()).collect();
        assert_eq!(
            automaton.recognise(input.clone()).next(),
            recogniser.recognise(input).next()
        );
    }
}

#[test]
fn test_tts_correctness() {
    let automaton = TreeStackAutomaton::from(pmcfg_from_file("examples/example.pmcfg"));
    let tts = TTSElement::new();
    let (tts_ed_automaton, _) = tts.approximate_automaton(&automaton);

    let true_positives_and_true_negatives = vec![
        "",
        "abcd",
        "aabccd",
        "aaabcccd",
    ];

    for word in true_positives_and_true_negatives {
        let input: Vec<_> = String::from(word).chars().map(|x| x.to_string()).collect();
        assert_eq!(
            automaton.recognise(input.clone()).next().is_some(),
            tts_ed_automaton.recognise(input).next().is_some()
        );
    }

    let false_positives = vec![
        "aabcd",
        "aabbcd",
        "abbcccdddd",
        "abbbccdddd",
    ];

    for word in false_positives {
        let input: Vec<_> = String::from(word).chars().map(|x| x.to_string()).collect();
        assert_eq!(false, automaton.recognise(input.clone()).next().is_some());
        assert_eq!(true, tts_ed_automaton.recognise(input).next().is_some());
    }
}

#[test]
fn test_pmcfg_from_str_correctness() {
    let rule_s0 = PMCFGRule {
        head: String::from("S"),
        tail: vec![String::from("A"), String::from("B")],
        composition: Composition { composition: vec![
            vec![VarT::Var(0, 0), VarT::Var(1, 0), VarT::Var(0, 1), VarT::Var(1, 1)],
        ] },
        weight: LogDomain::new(1.0).unwrap()
    };
    let rule_a0 = PMCFGRule {
        head: String::from("A"),
        tail: vec![String::from("A")],
        composition: Composition { composition: vec![
            vec![VarT::T(String::from("a")), VarT::Var(0, 0)],
            vec![VarT::T(String::from("c")), VarT::Var(0, 1)],
        ] },
        weight: LogDomain::new(0.5).unwrap()
    };
    let rule_a1 = PMCFGRule {
        head: String::from("A"),
        tail: vec![],
        composition: Composition { composition: vec![
            vec![],
            vec![],
        ] },
        weight: LogDomain::new(0.5).unwrap()
    };
    let rule_b0 = PMCFGRule {
        head: String::from("B"),
        tail: vec![String::from("B")],
        composition: Composition { composition: vec![
            vec![VarT::T(String::from("b")), VarT::Var(0, 0)],
            vec![VarT::T(String::from("d")), VarT::Var(0, 1)],
        ] },
        weight: LogDomain::new(0.5).unwrap()
    };
    let rule_b1 = PMCFGRule {
        head: String::from("B"),
        tail: vec![],
        composition: Composition { composition: vec![
            vec![],
            vec![],
        ] },
        weight: LogDomain::new(0.5).unwrap()
    };
    let control_grammar = PMCFG {
        initial: vec![String::from("S")],
        rules: vec![rule_s0, rule_a0, rule_a1, rule_b0, rule_b1]
    };

    let grammar = pmcfg_from_file("examples/example.pmcfg");
    assert_eq!(
        control_grammar.clone(),
        grammar.clone()
    );

    let control_automaton = TreeStackAutomaton::from(control_grammar);
    let automaton = TreeStackAutomaton::from(grammar);
    let words = vec![
        "",
        "aabccd",
        "aabbcd",
        "aabccdd",
    ];

    for word in words {
        let input: Vec<_> = String::from(word).chars().map(|x| x.to_string()).collect();
        assert_eq!(
            control_automaton.recognise(input.clone()).next().is_some(),
            automaton.recognise(input).next().is_some()
        );
    }
}
