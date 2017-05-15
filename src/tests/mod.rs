use std::marker::PhantomData;

use automata::*;
use cfg::*;
use pmcfg::*;
use approximation::*;
use util::integeriser::*;
use util::log_prob::*;


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

    let r0: PMCFGRule<String, String, LogProb> = PMCFGRule {
        head: "S".to_string(),
        tail: vec!["A".to_string(), "B".to_string()],
        composition: c0.clone(),
        weight: LogProb::new(1.0).unwrap(),
    };

    let r1: PMCFGRule<String, String, LogProb> = PMCFGRule {
        head: "A".to_string(),
        tail: Vec::new(),
        composition: c1.clone(),
        weight: LogProb::new(0.6).unwrap(),
    };

    let r2: PMCFGRule<String, String, LogProb> = PMCFGRule {
        head: "A".to_string(),
        tail: vec!["A".to_string()],
        composition: c2.clone(),
        weight: LogProb::new(0.4).unwrap(),
    };

    let r3: PMCFGRule<String, String, LogProb> = PMCFGRule {
        head: "B".to_string(),
        tail: Vec::new(),
        composition: c1.clone(),
        weight: LogProb::new(0.7).unwrap(),
    };

    let r4: PMCFGRule<String, String, LogProb> = PMCFGRule {
        head: "B".to_string(),
        tail: vec!["B".to_string()],
        composition: c3.clone(),
        weight: LogProb::new(0.3).unwrap(),
    };

    let r0_string = "\"S\" → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (\"A\", B)  # 1.0";
    let r1_string = "A → [[],[]] ()  # 0.6";
    let r2_string = "A → [[T a, Var 0 0],[T c, Var 0 1]] (A)  # 0.4";
    let r3_string = "B → [[],[]] ()  # 0.7";
    let r4_string = "B → [[T b, Var 0 0],[T d, Var 0 1]] (B)  # 0.3";

    assert_eq!(Ok(r0.clone()),
               r0_string.parse::<PMCFGRule<String, String, LogProb>>());
    assert_eq!(Ok(r1.clone()),
               r1_string.parse::<PMCFGRule<String, String, LogProb>>());
    assert_eq!(Ok(r2.clone()),
               r2_string.parse::<PMCFGRule<String, String, LogProb>>());
    assert_eq!(Ok(r3.clone()),
               r3_string.parse::<PMCFGRule<String, String, LogProb>>());
    assert_eq!(Ok(r4.clone()),
               r4_string.parse::<PMCFGRule<String, String, LogProb>>());

    let g: PMCFG<String, String, LogProb> = PMCFG {
        _dummy: PhantomData,
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

    assert_ne!(None, a.recognise(vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()]));
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

    let r0: CFGRule<String, String, LogProb> = CFGRule {
        head: "S".to_string(),
        composition: c0.clone(),
        weight: LogProb::new(1.0).unwrap(),
    };

    let r1: CFGRule<String, String, LogProb> = CFGRule {
        head: "A".to_string(),
        composition: c1.clone(),
        weight: LogProb::new(0.6).unwrap(),
    };

    let r2: CFGRule<String, String, LogProb> = CFGRule {
        head: "A".to_string(),
        composition: c2.clone(),
        weight: LogProb::new(0.4).unwrap(),
    };

    let r3: CFGRule<String, String, LogProb> = CFGRule {
        head: "B".to_string(),
        composition: c3.clone(),
        weight: LogProb::new(1.0).unwrap(),
    };

    let r0_string = "S → [Nt A] # 1";
    let r1_string = "A → [T a, Nt A, Nt B] # 0.6";
    let r2_string = "A → [T a] # 0.4";
    let r3_string = "B → [T b] # 1";

    assert_eq!(Ok(r0.clone()),
               r0_string.parse::<CFGRule<String, String, LogProb>>());
    assert_eq!(Ok(r1.clone()),
               r1_string.parse::<CFGRule<String, String, LogProb>>());
    assert_eq!(Ok(r2.clone()),
               r2_string.parse::<CFGRule<String, String, LogProb>>());
    assert_eq!(Ok(r3.clone()),
               r3_string.parse::<CFGRule<String, String, LogProb>>());

    let g: CFG<String, String, LogProb> = CFG {
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

    assert_ne!(None, a.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string()]));
}

#[test]
fn test_integeriser () {
    let arr1 = vec!["this", "is", "a", "test", "."];
    let arr2 = vec!["this", "test", "is", "really", "simple", "."];

    let mut integeriser = Integeriser::new();

    let mut arr1i = Vec::new();
    let mut arr2i = Vec::new();

    for a in arr1 {
        arr1i.push(integeriser.get_key(a));
    }

    for a in arr2 {
        arr2i.push(integeriser.get_key(a));
    }

    assert_eq!(arr1i[0], arr2i[0]);
    assert_eq!(arr1i[1], arr2i[2]);
    assert_eq!(arr1i[3], arr2i[1]);
    assert_eq!(arr1i[4], arr2i[5]);

    assert_ne!(arr1i[1], arr2i[0]);
    assert_ne!(arr1i[2], arr2i[1]);
    assert_ne!(arr1i[3], arr2i[3]);
}


fn test_equivalence(a : String)->String{
    match &*a{
        "A" => "C".to_string() ,
        "B" => "C".to_string() ,
        _ => a.clone(),
    }
}

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

    let g: CFG<String, String, LogProb> = g_string.parse().unwrap();

    let a = PushDownAutomaton::from(g);

    let rlb = RlbElement{
        dummy : PhantomData,
        func : test_equivalence,
    };

    let b = a.approximation(rlb).unwrap();

    assert_ne!(None, b.recognise(vec!["a".to_string() ]));
    assert_eq!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string() ]));
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string(), "b".to_string(), "a".to_string() ]));
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string() ]));

    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]));

}
