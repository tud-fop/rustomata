extern crate serde_json;
extern crate bincode;

use clap::{SubCommand, App, Arg, ArgMatches, ArgGroup};
use std::io::{stdin, stdout, Read};
use std::fs::File;

use PMCFG;
use log_domain::LogDomain;
use cs_representation::CSRepresentation;
use cs_representation::automata::{KellerGenerator, NaiveGenerator, NaiveFilterAutomaton, ApproxGenerator};

pub fn get_sub_command(name: &str) -> App {
    SubCommand::with_name(name)
                .about("Chomsky-Schützenberger representation of MCFGs")
                .subcommand(
                    SubCommand::with_name("extract")
                                .about("Reads a grammar from stdin and prints an object
                                       that Chomsky-Schützenberger-represents the grammar.")
                ).subcommand(
                    SubCommand::with_name("parse")
                                .about("Reads a list of words from stdin and parses them
                                       using the CS representation of a grammar.")
                                .arg(
                                    Arg::with_name("csfile")
                                        .required(true)
                                        .index(1)
                                        .help("The file that contains the CS representation of a grammar.")
                                ).arg(
                                    Arg::with_name("k")
                                        .short("k")
                                        .takes_value(true)
                                        .help("Puts the `k` in k-best parsing.")
                                ).arg(
                                    Arg::with_name("beam")
                                        .short("b").long("beam")
                                        .takes_value(true)
                                        .help("Beam with during search.")
                                )
                ).subcommand(
                    SubCommand::with_name("show")
                                .about("Shows details about the generator automaton and the partition
                                        associated with a Chomsky-Schützenberger representation of a gramar.")
                ).group(
                    ArgGroup::with_name("strategy")
                ).arg(
                    Arg::with_name("naive")
                        .help("Use a naive Generator automaton.")
                        .long("naive")
                        .group("strategy")
                ).arg(
                    Arg::with_name("approx")
                        .help("Use a regular approximation the Keller generator.")
                        .long("approx")
                        .group("strategy")
                        .takes_value(true)
                ).arg(
                    Arg::with_name("keller")
                        .help("Use a Keller automaton to generate candidates.")
                        .long("keller")
                        .group("strategy")
                )
}

pub fn handle_sub_matches(submatches: &ArgMatches) {
    enum Strat {
        Keller, Naive, Approx(usize)
    }

    let strategy = {
        if submatches.is_present("strategy") {
            if submatches.is_present("keller") {
                Strat::Keller
            } else if submatches.is_present("naive") {
                Strat::Naive
            } else if let Some(depth) = submatches.value_of("approx") {
                Strat::Approx(depth.parse().unwrap())
            } else {
                Strat::Keller
            }
        } else {
            Strat::Keller
        }
    };

    match submatches.subcommand() {
        ("extract", _) => {
            let mut grammar_string = String::new();
            stdin().read_to_string(&mut grammar_string).expect(
                "Could not read from stdin. Be sure to provide the gramar file as input.",
            );
            let grammar: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().expect(
                "Could not decode the grammar provided via stdin.",
            );

            match strategy {
                Strat::Keller => {
                    bincode::serialize_into(
                        &mut stdout(),
                        &CSRepresentation::<String, String, NaiveFilterAutomaton<String>, KellerGenerator>::new(KellerGenerator, grammar),
                        bincode::Infinite
                    ).unwrap()
                }, Strat::Naive => {
                    bincode::serialize_into(
                        &mut stdout(),
                        &CSRepresentation::<String, String, NaiveFilterAutomaton<String>, NaiveGenerator>::new(NaiveGenerator, grammar),
                        bincode::Infinite
                    ).unwrap()
                }, Strat::Approx(d) => {
                    bincode::serialize_into(
                        &mut stdout(),
                        &CSRepresentation::<String, String, NaiveFilterAutomaton<String>, ApproxGenerator>::new(ApproxGenerator(d), grammar),
                        bincode::Infinite
                    ).unwrap()
                }
            }
        }
        // ("show", Some(params)) => {
            
        //     let csrep: CSRepresentation<String, String, NaiveFilterAutomaton<String>> = 
        //         if let Some(filename) = params.value_of("file") {
        //             let mut file = File::open(filename).expect("Could not open file.");
        //             bincode::deserialize_from(&mut file, bincode::Infinite).expect("Could not deserialize contents of file.")
        //         } else {
        //             bincode::deserialize_from(&mut stdin(), bincode::Infinite).expect("Could not deserialize contents stdin.")
        //         };

        //     match params.subcommand() {
        //         ("automaton", Some(subparams)) => {
        //             if subparams.is_present("dump") {
        //                 csrep.generator.write_binary(&mut stdout()).expect("Could not write automaton to stdout.");
        //                 if let Some(sym) = subparams.value_of("symbols") {
        //                     csrep.generator.write_symbols(&mut File::create(sym).expect("Could not open file.")).expect("Could not write to file.");
        //                 }
        //             } else {
        //                 println!("{}", csrep.generator);
        //             }

        //         },
        //         _ => ()
        //     }


        // }
        ("parse", Some(params)) => {
            let mut word_strings = String::new();
            stdin().read_to_string(&mut word_strings).expect(
                "Please pass a string to parse using stdin or using the `--word` option.",
            );
            let k: usize = match params.value_of("k") {
                Some(n) => n.parse::<usize>().unwrap(),
                None => 1usize,
            };
            let beam: usize = match params.value_of("beam") {
                Some(b) => b.parse::<usize>().unwrap(),
                None => 2000usize,
            };
            let mut csfile = File::open(params.value_of("csfile").unwrap()).unwrap();
            
            match strategy {
                Strat::Keller => {
                    let csrep: CSRepresentation<String, String, NaiveFilterAutomaton<String>, KellerGenerator> = bincode::deserialize_from(&mut csfile, bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                            println!("{}", derivation);
                        }
                    }
                }, Strat::Naive => {
                    let csrep: CSRepresentation<String, String, NaiveFilterAutomaton<String>, NaiveGenerator> = bincode::deserialize_from(&mut csfile, bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                            println!("{}", derivation);
                        }
                    }
                }, Strat::Approx(_) => {
                    let csrep: CSRepresentation<String, String, NaiveFilterAutomaton<String>, ApproxGenerator> = bincode::deserialize_from(&mut csfile, bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                            println!("{}", derivation);
                        }
                    }
                }
            }
        },
        _ => (),
    }
}
