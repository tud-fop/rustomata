use std::marker::PhantomData;

use tree_stack::*;
use automata::*;


#[test]
fn from_str_automaton() {
    let i1: TreeStackInstruction<String>
        = TreeStackInstruction::Up {
            n: 1,
            current_val: "zwei".to_string(),
            old_val: "drei".to_string(),
            new_val: "vier".to_string()
        };

    let i2: TreeStackInstruction<String>
        = TreeStackInstruction::Push {
            n: 1,
            current_val: "zwei".to_string(),
            new_val: "vier".to_string()
        };

    let i3: TreeStackInstruction<String>
        = TreeStackInstruction::Down {
            current_val: "zwei".to_string(),
            old_val: "drei".to_string(),
            new_val: "vier".to_string()
        };

    let i1_string  = "Up 1 zwei drei vier";
    let i1_string2 = "   Up 1  zwei  drei   vier  ";
    let i2_string  = "Push 1 zwei vier";
    let i3_string  = "Down zwei drei vier";

    assert_eq!(Ok(i1.clone()), i1_string.parse() );
    assert_eq!(Ok(i1.clone()), i1_string2.parse());
    assert_eq!(Ok(i2.clone()), i2_string.parse() );
    assert_eq!(Ok(i3.clone()), i3_string.parse() );

    let t1: Transition<TreeStack<String>, TreeStackInstruction<String>, String, f64>
        = Transition {
            _dummy: PhantomData,
            word: vec!["hello".to_string(), "world".to_string()],
            weight: 0.5,
            instruction: i1
        };

    let t2: Transition<TreeStack<String>, TreeStackInstruction<String>, String, f64>
        = Transition {
            _dummy: PhantomData,
            word: vec!["\"hello\\".to_string(), "world".to_string()],
            weight: 0.5,
            instruction: i2
        };

    let t3: Transition<TreeStack<String>, TreeStackInstruction<String>, String, f64>
        = Transition {
            _dummy: PhantomData,
            word: vec!["\"hello\\".to_string(), "world".to_string()],
            weight: 0.5,
            instruction: i3
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

    let automaton: TreeStackAutomaton<String, String, f64>
        = TreeStackAutomaton::new(
            vec![t1.clone(), t2.clone(), t3.clone()],
            TreeStack::new("eins".to_string()),
        );

    let mut automaton_string: String = String::from("initial: eins\n\n");
    automaton_string.push_str(t1_string.as_str());
    automaton_string.push_str("\n");
    automaton_string.push_str(t2_string.as_str());
    automaton_string.push_str("\n");
    automaton_string.push_str(t3_string.as_str());
    automaton_string.push_str("\n");

    assert_eq!(Ok(automaton), automaton_string.parse())
}
