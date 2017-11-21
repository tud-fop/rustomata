use std::marker::PhantomData;

use automata::*;
use integerise::*;
use pmcfg::*;
use cfg::*;
use approximation::*;
use util::equivalence_classes::*;
use util::ctf::*;
use nfa::*;
use push_down_automaton::*;
use tree_stack_automaton::*;

use log_prob::LogProb;

use num_traits::One;


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

    let automaton: TreeStackAutomaton<String, String, LogProb<f64>> = TreeStackAutomaton::new(
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

    let automaton_parse: Result<TreeStackAutomaton<String, String, LogProb<f64>>, _> = automaton_string.parse();

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

    let r0: PMCFGRule<String, String, LogProb<f64>> = PMCFGRule {
        head: "S".to_string(),
        tail: vec!["A".to_string(), "B".to_string()],
        composition: c0.clone(),
        weight: LogProb::new(1.0).unwrap(),
    };

    let r1: PMCFGRule<String, String, LogProb<f64>> = PMCFGRule {
        head: "A".to_string(),
        tail: Vec::new(),
        composition: c1.clone(),
        weight: LogProb::new(0.6).unwrap(),
    };

    let r2: PMCFGRule<String, String, LogProb<f64>> = PMCFGRule {
        head: "A".to_string(),
        tail: vec!["A".to_string()],
        composition: c2.clone(),
        weight: LogProb::new(0.4).unwrap(),
    };

    let r3: PMCFGRule<String, String, LogProb<f64>> = PMCFGRule {
        head: "B".to_string(),
        tail: Vec::new(),
        composition: c1.clone(),
        weight: LogProb::new(0.7).unwrap(),
    };

    let r4: PMCFGRule<String, String, LogProb<f64>> = PMCFGRule {
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
               r0_string.parse::<PMCFGRule<String, String, LogProb<f64>>>());
    assert_eq!(Ok(r1.clone()),
               r1_string.parse::<PMCFGRule<String, String, LogProb<f64>>>());
    assert_eq!(Ok(r2.clone()),
               r2_string.parse::<PMCFGRule<String, String, LogProb<f64>>>());
    assert_eq!(Ok(r3.clone()),
               r3_string.parse::<PMCFGRule<String, String, LogProb<f64>>>());
    assert_eq!(Ok(r4.clone()),
               r4_string.parse::<PMCFGRule<String, String, LogProb<f64>>>());

    let g: PMCFG<String, String, LogProb<f64>> = PMCFG {
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

    let a = IntTreeStackAutomaton::from(g);

    assert_ne!(None, a.recognise(vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()]).next());
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

    let r0: CFGRule<String, String, LogProb<f64>> = CFGRule {
        head: "S".to_string(),
        composition: c0.clone(),
        weight: LogProb::one(),
    };

    let r1: CFGRule<String, String, LogProb<f64>> = CFGRule {
        head: "A".to_string(),
        composition: c1.clone(),
        weight: LogProb::new(0.6).unwrap(),
    };

    let r2: CFGRule<String, String, LogProb<f64>> = CFGRule {
        head: "A".to_string(),
        composition: c2.clone(),
        weight: LogProb::new(0.4).unwrap(),
    };

    let r3: CFGRule<String, String, LogProb<f64>> = CFGRule {
        head: "B".to_string(),
        composition: c3.clone(),
        weight: LogProb::one(),
    };

    let r0_string = "S → [Nt A] # 1";
    let r1_string = "A → [T a, Nt A, Nt B] # 0.6";
    let r2_string = "A → [T a] # 0.4";
    let r3_string = "B → [T b] # 1";

    assert_eq!(Ok(r0.clone()),
               r0_string.parse::<CFGRule<String, String, LogProb<f64>>>());
    assert_eq!(Ok(r1.clone()),
               r1_string.parse::<CFGRule<String, String, LogProb<f64>>>());
    assert_eq!(Ok(r2.clone()),
               r2_string.parse::<CFGRule<String, String, LogProb<f64>>>());
    assert_eq!(Ok(r3.clone()),
               r3_string.parse::<CFGRule<String, String, LogProb<f64>>>());

    let g: CFG<String, String, LogProb<f64>> = CFG {
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

    let a = IntPushDownAutomaton::from(g);

    assert_ne!(None, a.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string()]).next());
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

    let g: CFG<String, String, LogProb<f64>> = g_string.parse().unwrap();

    let a = IntPushDownAutomaton::from(g);

    let mut e_string = String::from("S [S]\n");
    e_string.push_str("N [A, B]\n");
    e_string.push_str("R [*]\n");

    let e: EquivalenceClass<String, String> = e_string.parse().unwrap();

    let rlb = RlbElement::new(e);

    let (b, _) = a.approximation(&rlb).unwrap();

    assert_ne!(None, b.recognise(vec!["a".to_string() ]).next());
    assert_eq!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string(), "b".to_string(), "a".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string() ]).next());

    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]).next());

}

#[test]
fn test_topk() {

    //create (and test) initial push down automata
    let r0_string = "S → [Nt A, Nt A, Nt A, Nt A, Nt A ] # 1";
    let r1_string = "A → [T a                         ] # 0.6";
    let r2_string = "A → [T b                         ] # 0.4";


    let mut g_string = String::from("initial: [S]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());

    let g: CFG<String, String, LogProb<f64>> = g_string.parse().unwrap();

    let a = IntPushDownAutomaton::from(g);

    let ptk = PDTopKElement::new(4);

    let (b, _) = a.clone().approximation(&ptk).unwrap();

    assert_eq!(None, a.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]).next());
    assert_ne!(None, a.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string()]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string()]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string()]).next());
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

    let g: PMCFG<String, String, LogProb<f64>> = g_string.parse().unwrap();

    let a = IntTreeStackAutomaton::from(g);

    let tts = TTSElement::new();

    let (b, _) = a.clone().approximation(&tts).unwrap();

    assert_ne!(None, a.recognise(vec!["a".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_eq!(None, a.recognise(vec!["a".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_ne!(None, b.recognise(vec!["a".to_string(), "a".to_string(), "e".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "d".to_string() ]).next());
    assert_eq!(None, b.recognise(vec!["a".to_string(), "e".to_string(), "e".to_string(), "b".to_string(), "c".to_string(), "c".to_string(), "d".to_string() ]).next());
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

    let g: CFG<String, String, LogProb<f64>> = g_string.parse().unwrap();

    let a = IntPushDownAutomaton::from(g);

    let mut e_string = String::from("S [S]\n");
    e_string.push_str("N [A, B]\n");
    e_string.push_str("R [*]\n");

    let e: EquivalenceClass<String, String> = e_string.parse().unwrap();

    let rlb = RlbElement::new(e);

    let (b, _) = a.approximation(&rlb).unwrap();

    let itemb = b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]).next().unwrap();
    assert_ne!(None, b.check_run(&itemb.give_up().1));

}

#[test]
fn test_ctf_scheme(){
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

    let g: PMCFG<String, String, LogProb<f64>> = g_string.parse().unwrap();

    let automaton = IntTreeStackAutomaton::from(g);

    let tts = TTSElement::new();

    let (a, ntts) = automaton.approximation(&tts).unwrap();

    let mut e_string = String::from("S [S]\n");
    e_string.push_str("N [A, B]\n");
    e_string.push_str("R [*]\n");

    let e: EquivalenceClass<String, String> = e_string.parse().unwrap();

    let rlb = RlbElement::new(e);

    let (b, nrlb) = a.approximation(&rlb).unwrap();

    let size = 5;

    let ptk = PDTopKElement::new(size);

    let (c, nptk) = b.approximation(&ptk).unwrap();

    let corpus = String::from("a a e e b c c d");

    let n1 = 1000;
    let n2 = 100;
    let n3 = 10;
    let n4 = 1;
    let mut c2 = 0;
    let mut c3 = 0;
    let mut c4 = 0;

    let mut recog = Vec::new();

    for sentence in corpus.lines() {
        println!("{}:\n", sentence);
        for parse1 in c.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect()).take(n1) {
            let s1 = ctf_level_i(parse1.give_up().1, &nptk, &b);
            for parse2 in s1{
                let s2 = ctf_level_i(parse2.give_up().1, &nrlb, &a);
                for parse3 in s2{
                    let s3 = ctf_level_i(parse3.give_up().1, &ntts, &automaton);
                    for parse4 in s3{
                        recog.push(parse4);
                        c4=c4+1;
                        if c4>=n4{
                            break
                        }
                    }
                    c3=c3+1;
                    if c4>=n4||c3>=n3{
                        break;
                    }
                }
                c2=c2+1;
                if c2>=n2||c3>=n3||c4>=n4{
                    break;
                }
            }
            if c2>=n2||c3>=n3||c4>=n4{
                break;
            }
        }
    }
    assert_eq!(false, recog.is_empty());
}

#[test]
fn test_ptk_to_nfa(){
    //create (and test) initial push down automata
    let r0_string = "S → [Nt A, Nt A, Nt A, Nt A, Nt A ] # 1";
    let r1_string = "A → [T a                         ] # 0.6";
    let r2_string = "A → [T b                         ] # 0.4";


    let mut g_string = String::from("initial: [S]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());

    let g: CFG<String, String, LogProb<f64>> = g_string.parse().unwrap();

    let a = PushDownAutomaton::from(g);

    let ptk = PDTopKElement::new(4);

    let (b, _) = a.clone().approximation(&ptk).unwrap();

    let n = from_pd(&b);

    match n {
        Some((nfa, nfa_dict))=>{
            assert!(true);
            assert_eq!(None, a.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]).next());
            assert_ne!(None, nfa.recognise(&vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string() ]).next());
            assert_ne!(None, a.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string()]).next());
            assert_ne!(None, nfa.recognise(&vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string()]).next());
            assert_ne!(None, nfa.recognise(&vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string()]).next());
            let m_obj = match nfa.recognise(&vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string()]).next(){
                Some(x) =>{Some(nfa_dict.translate(x.1))},
                None => None,
            };
            let n_obj: Option<Vec<_>> = match b.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "a".to_string(), "a".to_string()]).next(){
                Some(x) => Some(x.1.into()),
                None => None,
            };
            assert_eq!(n_obj, m_obj);
        },
        None=>{
            assert!(false);
        },
    }

}
