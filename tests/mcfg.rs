extern crate log_domain;
extern crate rustomata;

use log_domain::LogDomain;
use std::fs::File;
use std::io::Read;

use rustomata::pmcfg::{PMCFG, PMCFGRule, separate_terminal_rules};
use rustomata::pmcfg::negra::to_negra;
use rustomata::recognisable::Recognisable;
use rustomata::tree_stack_automaton::*;
use rustomata::tree_stack_automaton::{PosState, TreeStackAutomaton};

fn example_tree_stack_automaton()
        -> TreeStackAutomaton<PosState<PMCFGRule<String, String, LogDomain<f64>>>, String, LogDomain<f64>>
{
    let mut grammar_file = File::open("examples/example.mcfg").unwrap();
    let mut grammar_string = String::new();
    let _ = grammar_file.read_to_string(&mut grammar_string);
    let grammar: PMCFG<String, String, _> = grammar_string.parse().unwrap();

    let automaton = TreeStackAutomaton::from(grammar);
    automaton
}

#[test]
fn test_example_mcfg_to_negra() {
    let automaton = example_tree_stack_automaton();
    let tree_stack = automaton.recognise(vec![
        String::from("a"), String::from("a"), String::from("b"),
        String::from("c"), String::from("c"), String::from("d")
    ]).next().unwrap().0;

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
