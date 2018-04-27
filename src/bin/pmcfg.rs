use clap::{Arg, ArgMatches, App, SubCommand};
use log_domain::LogDomain;
use rustomata::grammars::pmcfg::PMCFG;
use rustomata::grammars::pmcfg::negra::{to_negra, DumpMode};
use rustomata::recognisable::Recognisable;
use rustomata::automata::tree_stack_automaton::TreeStackAutomaton;
use rustomata::automata::tree_stack_automaton::to_abstract_syntax_tree;
use rustomata::util::reverse::Reverse;

use std::io::{self, Read};
use std::fs::File;

pub fn get_sub_command() -> App<'static, 'static> {
    SubCommand::with_name("mcfg")
        .author("Tobias Denkinger <tobias.denkinger@tu-dresden.de>")
        .about("functions related to multiple context-free grammars")
        .subcommand(
            SubCommand::with_name("parse")
                .author("Tobias Denkinger <tobias.denkinger@tu-dresden.de>")
                .about("parses from stdin with a multiple context-free grammar")
                .arg(
                    Arg::with_name("grammar")
                        .help("grammar file to use")
                        .index(1)
                        .required(true),
                )
                .arg(
                    Arg::with_name("number-of-parses")
                        .help("number of parses that should be returned")
                        .short("n")
                        .long("number")
                        .value_name("number-of-parses")
                        .default_value("1")
                        .required(false),
                )
                .arg(
                    Arg::with_name("beam-width")
                        .help("maximum number of frontier nodes in the search space")
                        .short("b")
                        .long("beam")
                        .value_name("beam-width")
                        .required(false),
                )
                .arg(
                    Arg::with_name("negra")
                        .help("turn on output in NeGra export format")
                        .long("negra"),
                ),
        )
        .subcommand(
            SubCommand::with_name("automaton")
                .author("Tobias Denkinger <tobias.denkinger@tu-dresden.de>")
                .about("constructs a tree-stack automaton from the given multiple context-free grammar")
                .arg(
                    Arg::with_name("grammar")
                        .help("grammar file to use")
                        .index(1)
                        .required(true),
                ),
        )
}

pub fn handle_sub_matches(mcfg_matches: &ArgMatches) {
    match mcfg_matches.subcommand() {
        ("parse", Some(mcfg_parse_matches)) => {
            let grammar_file_name = mcfg_parse_matches.value_of("grammar").unwrap();
            let mut grammar_file = File::open(grammar_file_name).unwrap();
            let n = mcfg_parse_matches
                .value_of("number-of-parses")
                .unwrap()
                .parse()
                .unwrap();
            let mut grammar_string = String::new();
            let _ = grammar_file.read_to_string(&mut grammar_string);
            let grammar: PMCFG<String, String, Reverse<LogDomain<f64>>> = grammar_string.parse().unwrap();

            let automaton = TreeStackAutomaton::from(grammar);

            let mut corpus = String::new();
            let _ = io::stdin().read_to_string(&mut corpus);

            for (i, sentence) in corpus.lines().enumerate() {
                let word = sentence.split_whitespace().map(|x| x.to_string()).collect();
                match mcfg_parse_matches.value_of("beam-width") {
                    Some(b) => {
                        for parse in automaton
                            .recognise_beam_search(b.parse().unwrap(), word)
                            .take(n)
                        {
                            let ast = to_abstract_syntax_tree(parse.0.storage.to_tree());
                            if mcfg_parse_matches.is_present("negra") {
                                println!("{}", to_negra(&ast, i + 1, DumpMode::Default));
                            } else {
                                println!("{}", parse.0);
                            }
                        }
                    }
                    None => {
                        for parse in automaton.recognise(word).take(n) {
                            let ast = to_abstract_syntax_tree(parse.0.storage.to_tree());
                            if mcfg_parse_matches.is_present("negra") {
                                println!("{}", to_negra(&ast, i + 1, DumpMode::Default));
                            } else {
                                println!("{}", parse.0);
                            }
                        }
                    }
                };
                println!();
            }
        }
        ("automaton", Some(mcfg_automaton_matches)) => {
            let grammar_file_name = mcfg_automaton_matches.value_of("grammar").unwrap();
            let mut grammar_file = File::open(grammar_file_name).unwrap();
            let mut grammar_string = String::new();
            let _ = grammar_file.read_to_string(&mut grammar_string);
            let grammar: PMCFG<String, String, Reverse<LogDomain<f64>>> = grammar_string.parse().unwrap();
            let automaton = TreeStackAutomaton::from(grammar);
            println!("{}", automaton);
        }
        _ => (),
    }
}
