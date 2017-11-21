extern crate serde_json;
extern crate bincode;

use clap::{SubCommand, App, Arg, ArgMatches};
use std::io::{stdin, stdout, Read};
use std::fs::{File};

use PMCFG;
use log_prob::LogProb;
use cs_representation::CSRepresentation;

pub fn get_sub_command(name: &str) -> App {
    SubCommand::with_name(name)
        .about("Chomsky-Schützenberger representation of MCFGs")
            .subcommand(SubCommand::with_name("from-mcfg")
                .about("Reads a grammar from stdin and prints an objects that represents the grammar.")
                .arg(Arg::with_name("pretty")
                    .short("p").long("pretty")
                    .takes_value(false)
                    .help("Prints a readable format (json).")
                )
            )
            .subcommand(SubCommand::with_name("parse")
                .about("Reads a list of words from stdin and parses them using the CS representation of a grammar.")
                .arg(Arg::with_name("csfile")
                    .required(true)
                    .index(1)
                    .help("The file that contains the CS representatino of a grammar.")
                )
                .arg(Arg::with_name("step")
                    .required(false)
                    .short("s").long("step-size")
                    .takes_value(true)
                    .help("The number of candidates generated before they are checked.")
                )
                .arg(Arg::with_name("trees")
                    .required(false)
                    .short("n").long("n-best")
                    .takes_value(true)
                    .help("The maximum amount of parse trees.")
                )
            )
}

pub fn handle_sub_matches(submatches: &ArgMatches) {
    match submatches.subcommand() {
        ("from-mcfg", Some(params)) => {
            let mut grammar_string = String::new();
            stdin().read_to_string(&mut grammar_string);
            let grammar: PMCFG<String, String, LogProb<f64>> = grammar_string.parse().unwrap();

            let csrep: CSRepresentation<String, String> = CSRepresentation::new(grammar);

            if params.is_present("pretty") {
                println!("{:?}", csrep.generator.to_arcs());
            } else {
                bincode::serialize_into(&mut stdout(), &csrep, bincode::Infinite).unwrap();
            }
        },
        ("parse", Some(params)) => {
            let mut word_strings = String::new();
            stdin().read_to_string(&mut word_strings);

            let mut csfile = File::open(params.value_of("csfile").unwrap()).unwrap();
            let csrep: CSRepresentation<String, String> = bincode::deserialize_from(&mut csfile, bincode::Infinite).unwrap();

            let step: u8 = match params.value_of("step") {
                Some(n) => n.parse::<u8>().unwrap(),
                None => 5u8
            };
            let n: usize = match params.value_of("trees") {
                Some(n) => n.parse::<usize>().unwrap(),
                None => 1usize
            };
            
            for line in word_strings.lines() {
                eprintln!("{:?}", line);
                let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                for derivation in csrep.generate(words, step).take(n) {
                    println!("{}", derivation);
                }
            } 
        },
        _ => ()
    }
}