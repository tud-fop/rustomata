use clap::{Arg, ArgMatches, App, SubCommand};
use log_domain::LogDomain;
use cfg::CFG;
use integerise::{IntegerisedAutomaton, IntPushDownAutomaton};

use std::io::{self, Read};
use std::fs::File;

pub fn get_sub_command() -> App<'static, 'static> {
    SubCommand::with_name("cfg")
                    .about("functions related to context-free grammars")
                    .subcommand(SubCommand::with_name("parse")
                                .about("parses a word given a multiple context-free grammar")
                                .arg(Arg::with_name("grammar")
                                     .help("grammar file to use")
                                     .index(1)
                                     .required(true))
                                .arg(Arg::with_name("number-of-parses")
                                     .help("number of parses that should be returned")
                                     .short("n")
                                     .long("number")
                                     .default_value("1")
                                     .required(false))
                                .arg(Arg::with_name("beam-width")
                                     .help("maximum number of frontier nodes in the search space")
                                     .short("b")
                                     .long("beam")
                                     .value_name("beam-width")
                                     .required(false)))
                    .subcommand(SubCommand::with_name("automaton")
                                .about("constructs a tree-stack automaton from the given multiple context-free grammar")
                                .arg(Arg::with_name("grammar")
                                        .help("grammar file to use")
                                        .index(1)
                                     .required(true)))
}

pub fn handle_sub_matches(cfg_matches: &ArgMatches) {
    match cfg_matches.subcommand() {
        ("parse", Some(cfg_parse_matches)) => {
            let grammar_file_name = cfg_parse_matches.value_of("grammar").unwrap();
            let mut grammar_file = File::open(grammar_file_name).unwrap();
            let n = cfg_parse_matches
                .value_of("number-of-parses")
                .unwrap()
                .parse()
                .unwrap();
            let mut grammar_string = String::new();
            let _ = grammar_file.read_to_string(&mut grammar_string);
            let grammar: CFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();

            let automaton = IntPushDownAutomaton::from(grammar);

            let mut corpus = String::new();
            let _ = io::stdin().read_to_string(&mut corpus);

            for sentence in corpus.lines() {
                let word = sentence.split_whitespace().map(|x| x.to_string()).collect();
                match cfg_parse_matches.value_of("beam-width") {
                    Some(b) => {
                        for parse in automaton
                            .recognise_beam_search(b.parse().unwrap(), word)
                            .take(n)
                        {
                            println!("{:?}", parse.translate().0);
                        }
                    }
                    None => {
                        for parse in automaton.recognise(word).take(n) {
                            println!("{:?}", parse.translate().0);
                        }
                    }
                };
                println!();
            }
        }
        ("automaton", Some(cfg_automaton_matches)) => {
            let grammar_file_name = cfg_automaton_matches.value_of("grammar").unwrap();
            let mut grammar_file = File::open(grammar_file_name).unwrap();
            let mut grammar_string = String::new();
            let _ = grammar_file.read_to_string(&mut grammar_string);
            let grammar: CFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();

            let automaton = IntPushDownAutomaton::from(grammar);
            println!("{}", automaton);
        }
        _ => (),
    }
}
