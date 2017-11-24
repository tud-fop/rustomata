use clap::{Arg, ArgMatches, App, SubCommand};
use log_domain::LogDomain;
use automata::Automaton;
use tree_stack_automaton::TreeStackAutomaton;

use std::io::{self, Read};
use std::fs::File;

pub fn get_sub_command() -> App<'static, 'static> {
    SubCommand::with_name("tsa")
        .about("functions related to tree-stack automata")
        .subcommand(
            SubCommand::with_name("recognise")
                .about("recognises from stdin with a tree-stack automaton")
                .arg(
                    Arg::with_name("automaton")
                        .help("automaton file to use")
                        .index(1)
                        .required(true),
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
                    Arg::with_name("number-of-runs")
                        .help("number of runs that should be returned")
                        .short("n")
                        .long("number")
                        .default_value("1")
                        .required(false),
                ),
        )
}

pub fn handle_sub_matches(tsa_matches: &ArgMatches) {
    match tsa_matches.subcommand() {
        ("recognise", Some(tsa_recognise_matches)) => {
            let automaton_file_name = tsa_recognise_matches.value_of("automaton").unwrap();
            let mut automaton_file = File::open(automaton_file_name).unwrap();
            let n = tsa_recognise_matches
                .value_of("number-of-runs")
                .unwrap()
                .parse()
                .unwrap();
            let mut automaton_string = String::new();
            let _ = automaton_file.read_to_string(&mut automaton_string);
            let automaton: TreeStackAutomaton<String, String, LogDomain<f64>> =
                automaton_string.parse().unwrap();

            let mut corpus = String::new();
            let _ = io::stdin().read_to_string(&mut corpus);

            for sentence in corpus.lines() {
                let word = sentence.split_whitespace().map(|x| x.to_string()).collect();
                match tsa_recognise_matches.value_of("beam-width") {
                    Some(b) => {
                        for run in automaton
                            .recognise_beam_search(b.parse().unwrap(), word)
                            .take(n)
                        {
                            println!("{:?}", run.1);
                        }
                    }
                    None => {
                        for run in automaton.recognise(word).take(n) {
                            println!("{:?}", run.1);
                        }
                    }
                };
                println!();
            }
        }
        _ => (),
    }
}
