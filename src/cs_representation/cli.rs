extern crate serde_json;
extern crate bincode;

use clap::{SubCommand, App, Arg, ArgMatches, ArgGroup};
use std::io::{stdin, stdout, Read};
use std::fs::File;

use PMCFG;
use log_domain::LogDomain;
use cs_representation::CSRepresentation;
use cs_representation::generator_automaton::{NaiveGeneratorAutomaton, ApproxGeneratorAutomaton};

pub fn get_sub_command(name: &str) -> App {
    SubCommand::with_name(name)
        .about("Chomsky-Schützenberger representation of MCFGs")
            .subcommand(SubCommand::with_name("from-mcfg")
                .about("Reads a grammar from stdin and prints an object
                        that Chomsky-Schützenberger-represents the grammar.")
                .group(ArgGroup::with_name("strategy"))
                .arg(Arg::with_name("naive")
                    .help("Use a naive Generator automaton.")
                    .long("naive")
                    .group("strategy")
                )
                .arg(Arg::with_name("approx")
                    .help("Use a regular approximation of a Dyck language as Generator automaton.")
                    .long("dyck-approximation")
                    .group("strategy")
                    .takes_value(true)
                )
            )
            .subcommand(SubCommand::with_name("parse")
                .about("Reads a list of words from stdin and parses them
                        using the CS representation of a grammar.")
                .arg(Arg::with_name("csfile")
                    .required(true)
                    .index(1)
                    .help("The file that contains the CS representation of a grammar.")
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
                .arg(Arg::with_name("word")
                    .required(false)
                    .short("w").long("word")
                    .takes_value(true)
                    .help("The word to parse.")
                )
            )
            .subcommand(SubCommand::with_name("show")
                .about("Shows details about the generator automaton and the partition
                        associated with a Chomsky-Schützenberger representation of a gramar.")
                .arg(Arg::with_name("file")
                    .required(false)
                    .short("f").long("file")
                    .takes_value(true)
                    .help("The file that contains the CS representation of a grammar.")
                )
                .subcommand(SubCommand::with_name("automaton")
                    .about("Show details about the Generator automaton.")
                    .arg(Arg::with_name("dump")
                        .help("Dump a binary file readable using OpenFst.")
                        .short("b").long("binary")
                    )
                    .arg(Arg::with_name("symbols")
                        .help("Dump the symbol table to the specified file.")
                        .takes_value(true)
                        .requires("dump")
                        .short("s").long("symbols")
                    )
                )
                .subcommand(SubCommand::with_name("partition")
                    .about("Show the partition that represents the multiple Dyck language.")
                )
            )
}

pub fn handle_sub_matches(submatches: &ArgMatches) {
    match submatches.subcommand() {
        ("from-mcfg", Some(params)) => {
            let mut grammar_string = String::new();
            stdin().read_to_string(&mut grammar_string).expect(
                "Could not read from stdin. Be sure to provide the gramar file as input.",
            );
            let grammar: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().expect(
                "Could not decode the grammar provided via stdin.",
            );

            let csrep: CSRepresentation<String, String> = {
                if params.is_present("strategy") {
                    if params.is_present("naive"){
                        CSRepresentation::new(NaiveGeneratorAutomaton, grammar)
                    } else if let Some(depth) = params.value_of("approx") {
                        let strat = ApproxGeneratorAutomaton(depth.parse::<usize>().expect("Please pass a natural number along with `--dyck-approximation`"));
                        CSRepresentation::new(strat, grammar)
                    } else {
                        CSRepresentation::new(ApproxGeneratorAutomaton(1), grammar)
                    }
                } else {
                    CSRepresentation::new(ApproxGeneratorAutomaton(1), grammar)
                }
            };

            if params.is_present("pretty") {
                println!("{:?}", csrep.generator.to_arcs());
            } else {
                bincode::serialize_into(&mut stdout(), &csrep, bincode::Infinite).unwrap();
            }
        }
        ("show", Some(params)) => {
            
            let csrep: CSRepresentation<String, String> = 
                if let Some(filename) = params.value_of("file") {
                    let mut file = File::open(filename).expect("Could not open file.");
                    bincode::deserialize_from(&mut file, bincode::Infinite).expect("Could not deserialize contents of file.")
                } else {
                    bincode::deserialize_from(&mut stdin(), bincode::Infinite).expect("Could not deserialize contents stdin.")
                };

            match params.subcommand() {
                ("automaton", Some(subparams)) => {
                    if subparams.is_present("dump") {
                        csrep.generator.write_binary(&mut stdout()).expect("Could not write automaton to stdout.");
                        if let Some(sym) = subparams.value_of("symbols") {
                            csrep.generator.write_symbols(&mut File::create(sym).expect("Could not open file.")).expect("Could not write to file.");
                        }
                    } else {
                        println!("{}", csrep.generator);
                    }

                },
                _ => ()
            }


        }
        ("parse", Some(params)) => {
            let mut word_strings = String::new();
            if let Some(word) = params.value_of("word") {
                word_strings.push_str(word);
            } else {
                stdin().read_to_string(&mut word_strings).expect(
                    "Please pass a string to parse using stdin or using the `--word` option.",
                );
            }

            let mut csfile = File::open(params.value_of("csfile").unwrap()).unwrap();
            let csrep: CSRepresentation<String, String> =
                bincode::deserialize_from(&mut csfile, bincode::Infinite).unwrap();

            let step: usize = match params.value_of("step") {
                Some(n) => n.parse::<usize>().unwrap(),
                None => 100usize,
            };
            let n: usize = match params.value_of("trees") {
                Some(n) => n.parse::<usize>().unwrap(),
                None => 1usize,
            };

            for line in word_strings.lines() {
                let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                for derivation in csrep.generate(words, step).take(n) {
                    println!("{}", derivation);
                }
            }
        }
        _ => (),
    }
}
