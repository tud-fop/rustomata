use std::marker::PhantomData;

use tree_stack::*;
use automata::*;
use pmcfg::*;


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
        _dummy: PhantomData,
        word: vec!["hello".to_string(), "world".to_string()],
        weight: LogProb::new(0.5).unwrap(),
        instruction: i1,
    };

    let t2 = Transition {
        _dummy: PhantomData,
        word: vec!["\"hello\\".to_string(), "world".to_string()],
        weight: LogProb::new(0.5).unwrap(),
        instruction: i2,
    };

    let t3 = Transition {
        _dummy: PhantomData,
        word: vec!["\"hello\\".to_string(), "world".to_string()],
        weight: LogProb::new(0.5).unwrap(),
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

    let automaton: TreeStackAutomaton<String, String, LogProb> = TreeStackAutomaton::new(
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

    let automaton_parse: Result<TreeStackAutomaton<String, String, LogProb>, _> = automaton_string.parse();

    assert_eq!(
        automaton.list_transitions(),
        automaton_parse.clone().unwrap().list_transitions()
    );

    assert_eq!(
        automaton.initial,
        automaton_parse.unwrap().initial
    );
}


#[test]
fn from_str_pmcfg() {
    assert_eq!(Ok(VarT::Var(0,1)), "Var 0 1".parse::<VarT<String>>());
    assert_eq!(Ok(VarT::T("test".to_string())), "T test".parse());

    let c1: Composition<String>
        = Composition{ composition: vec![
            vec![],
            vec![]
        ] };

    let c2
        = Composition{ composition: vec![
            vec![VarT::T("a".to_string()), VarT::Var(0,0), VarT::T("b".to_string())],
            vec![VarT::T("c".to_string()), VarT::Var(0,1), VarT::T("d".to_string())]
        ] };

    let c3
        = Composition{ composition: vec![
            vec![VarT::T("a".to_string())],
            vec![VarT::T("c".to_string())]
        ] };

    assert_eq!(Ok(c1.clone()), "[[],[]]".parse::<Composition<String>>());
    assert_eq!(Ok(c2.clone()), "[[T a, Var 0 0, T b],[T c, Var 0 1, T d]]".parse::<Composition<String>>());
    assert_eq!(Ok(c3.clone()), "[[T a],[T c]]".parse::<Composition<String>>());

    let r1: PMCFGRule<String, String, i32>
        = PMCFGRule {
            head: "A".to_string(),
            tail: Vec::new(),
            composition: c1.clone(),
            weight: 0
        };

    let r2: PMCFGRule<String, String, i32>
        = PMCFGRule {
            head: "A".to_string(),
            tail: vec!["A".to_string()],
            composition: c2.clone(),
            weight: -2
        };

    let r3: PMCFGRule<String, String, i32>
        = PMCFGRule {
            head: "A".to_string(),
            tail: vec!["A".to_string(), "B".to_string()],
            composition: c2.clone(),
            weight: -2
        };

    let r1_string = "A → [[],[]] ()  # 0";
    let r2_string = "A → [[T a, Var 0 0, T b],[T c, Var 0 1, T d]] (A)  # -2";

    assert_eq!(Ok(r1.clone()), r1_string.parse::<PMCFGRule<String, String, i32>>());
    assert_eq!(Ok(r1.clone()), r1_string.parse::<PMCFGRule<String, String, i32>>());
    assert_eq!(Ok(r2.clone()), r2_string.parse::<PMCFGRule<String, String, i32>>());
    assert_eq!(Ok(r3.clone()), "A    →    [[T a, Var 0 0, T b],[T c, Var 0 1, T d]]      (A, B)     #    -2".parse::<PMCFGRule<String, String, i32>>());

    let g: PMCFG<String, String, i32>
        = PMCFG {
            _dummy: PhantomData,
            initial: "A".to_string(),
            rules: vec![r1.clone(), r2.clone()]
        };

    let mut g_string = String::from("initial: A\n\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());

    assert_eq!(Ok(g), g_string.parse());
