extern crate bincode;

use clap::{SubCommand, App, Arg, ArgMatches, ArgGroup};
use std::io::{stdin, stdout, Read};
use std::fs::File;

use PMCFG;
use log_domain::LogDomain;
use mcfg::cs_representation::CSRepresentation;
use mcfg::cs_representation::automata::{KellerGenerator, NaiveGenerator, ApproxGenerator, NaiveFilterAutomaton, InsideFilterAutomaton};
use util::agenda::Capacity;

use flate2::{read, write, Compression};

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
                                ).arg(
                                    Arg::with_name("debugmode")
                                        .short("d").long("debug")
                                )
                ).group(
                    ArgGroup::with_name("strategy")
                ).arg(
                    Arg::with_name("naive")
                        .help("Use a naive Generator automaton.")
                        .long("naive")
                        .group("strategy")
                ).arg(
                    Arg::with_name("approx")
                        .help("Use a regular approximation the push-down generator.")
                        .long("approx")
                        .group("strategy")
                        .takes_value(true)
                ).arg(
                    Arg::with_name("keller")
                        .help("Use a push-down automaton to generate candidates.")
                        .long("keller")
                        .group("strategy")
                ).group(
                    ArgGroup::with_name("filter")
                ).arg(
                    Arg::with_name("naive-filter")
                        .help("Use a naive filter automaton.")
                        .long("naive-filter")
                        .group("filter")
                ).arg(
                    Arg::with_name("inside-filter")
                        .help("Use the inside filter automaton.")
                        .long("inside-filter")
                        .group("filter")
                )
}

pub fn handle_sub_matches(submatches: &ArgMatches) {
    enum Strat {
        Keller, Naive, Approx(usize)
    }
    enum Filter {
        Naive, Inside
    }

    let strategy = {
        if submatches.is_present("naive") {
            Strat::Naive
        } else if let Some(depth) = submatches.value_of("approx") {
                Strat::Approx(depth.parse().unwrap())
        } else {
            Strat::Keller
        }
    };
    let filter = {
        if submatches.is_present("naive-filter")  {
            Filter::Naive
        } else {
            Filter::Inside
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

            match (strategy, filter) {
                (Strat::Keller, Filter::Inside) => {
                    bincode::serialize_into(
                        &mut write::GzEncoder::new(stdout(), Compression::best()),
                        &CSRepresentation::<String, String, InsideFilterAutomaton<String>, KellerGenerator>::new(KellerGenerator, grammar),
                        bincode::Infinite
                    ).unwrap()
                }, (Strat::Naive, Filter::Inside) => {
                    bincode::serialize_into(
                        &mut write::GzEncoder::new(stdout(), Compression::best()),
                        &CSRepresentation::<String, String, InsideFilterAutomaton<String>, NaiveGenerator>::new(NaiveGenerator, grammar),
                        bincode::Infinite
                    ).unwrap()
                }, (Strat::Approx(d), Filter::Inside) => {
                    bincode::serialize_into(
                        &mut write::GzEncoder::new(stdout(), Compression::best()),
                        &CSRepresentation::<String, String, InsideFilterAutomaton<String>, ApproxGenerator>::new(ApproxGenerator(d), grammar),
                        bincode::Infinite
                    ).unwrap()
                },(Strat::Keller, Filter::Naive) => {
                    bincode::serialize_into(
                        &mut write::GzEncoder::new(stdout(), Compression::best()),
                        &CSRepresentation::<String, String, NaiveFilterAutomaton<String>, KellerGenerator>::new(KellerGenerator, grammar),
                        bincode::Infinite
                    ).unwrap()
                }, (Strat::Naive, Filter::Naive) => {
                    bincode::serialize_into(
                        &mut write::GzEncoder::new(stdout(), Compression::best()),
                        &CSRepresentation::<String, String, NaiveFilterAutomaton<String>, NaiveGenerator>::new(NaiveGenerator, grammar),
                        bincode::Infinite
                    ).unwrap()
                }, (Strat::Approx(d), Filter::Naive) => {
                    bincode::serialize_into(
                        &mut write::GzEncoder::new(stdout(), Compression::best()),
                        &CSRepresentation::<String, String, NaiveFilterAutomaton<String>, ApproxGenerator>::new(ApproxGenerator(d), grammar),
                        bincode::Infinite
                    ).unwrap()
                }
            }
        }

        ("parse", Some(params)) => {
            let mut word_strings = String::new();
            stdin().read_to_string(&mut word_strings).expect(
                "Please pass a string to parse using stdin or using the `--word` option.",
            );
            let k: usize = match params.value_of("k") {
                Some(n) => n.parse::<usize>().unwrap(),
                None => 1usize,
            };
            let beam: Capacity = match params.value_of("beam") {
                Some(b) => Capacity::Limit(b.parse().unwrap()),
                None => Capacity::Infinite,
            };
            let csfile = File::open(params.value_of("csfile").unwrap()).unwrap();
            
            match (strategy, filter) {
                (Strat::Keller, Filter::Inside) => {
                    let csrep: CSRepresentation<String, String, InsideFilterAutomaton<String>, KellerGenerator> = bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        if params.is_present("debugmode") {
                            csrep.debug(words.as_slice(), beam);
                        } else {
                            for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                                println!("{}", derivation);
                            }
                        }
                    }
                }, (Strat::Naive, Filter::Inside) => {
                    let csrep: CSRepresentation<String, String, InsideFilterAutomaton<String>, NaiveGenerator> = bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        if params.is_present("debugmode") {
                            csrep.debug(words.as_slice(), beam);
                        } else {
                            for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                                println!("{}", derivation);
                            }
                        }
                    }
                }, (Strat::Approx(_), Filter::Inside) => {
                    let csrep: CSRepresentation<String, String, InsideFilterAutomaton<String>, ApproxGenerator> = bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        if params.is_present("debugmode") {
                            csrep.debug(words.as_slice(), beam);
                        } else {
                            for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                                println!("{}", derivation);
                            }
                        }
                    }
                }, (Strat::Keller, Filter::Naive) => {
                    let csrep: CSRepresentation<String, String, NaiveFilterAutomaton<String>, KellerGenerator> = bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        if params.is_present("debugmode") {
                            csrep.debug(words.as_slice(), beam);
                        } else {
                            for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                                println!("{}", derivation);
                            }
                        }
                    }
                }, (Strat::Naive, Filter::Naive) => {
                    let csrep: CSRepresentation<String, String, NaiveFilterAutomaton<String>, NaiveGenerator> = bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        if params.is_present("debugmode") {
                            csrep.debug(words.as_slice(), beam);
                        } else {
                            for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                                println!("{}", derivation);
                            }
                        }
                    }
                }, (Strat::Approx(_), Filter::Naive) => {
                    let csrep: CSRepresentation<String, String, NaiveFilterAutomaton<String>, ApproxGenerator> = bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite).unwrap();
                    for line in word_strings.lines() {
                        let words: Vec<String> = line.split_whitespace().map(|s| s.to_string()).collect();

                        if params.is_present("debugmode") {
                            csrep.debug(words.as_slice(), beam);
                        } else {
                            for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                                println!("{}", derivation);
                            }
                        }
                    }
                }, 
            }
        },
        _ => (),
    }
}
