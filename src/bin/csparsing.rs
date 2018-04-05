use clap::{SubCommand, App, Arg, ArgMatches, ArgGroup};
use std::io::{stdin, stdout, Read};
use std::fs::File;
extern crate bincode;

use rustomata::lcfrs::Lcfrs;
use log_domain::LogDomain;
use rustomata::lcfrs::cs_representation::CSRepresentation;
use rustomata::lcfrs::cs_representation::automata::{FilterStrategy, GeneratorStrategy};
use rustomata::util::agenda::Capacity;
use rustomata::pmcfg::negra::to_negra;

use flate2::{read, write, Compression};

pub fn get_sub_command(name: &str) -> App {
    SubCommand::with_name(name)
                .about("Chomsky-Schützenberger representation of LCFRS")
                .subcommand(
                    SubCommand::with_name("extract")
                                .about("Reads a grammar from stdin and prints an object
                                       that Chomsky-Schützenberger-represents the grammar.")
                                .group(
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
                                    Arg::with_name("pd")
                                        .help("Use a push-down automaton to generate candidates.")
                                        .long("pd")
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
                )
}

pub fn handle_sub_matches(submatches: &ArgMatches) {
    match submatches.subcommand() {
        ("extract", Some(susbsubmatches)) => {
            let strategy = {
                if susbsubmatches.is_present("naive") {
                    GeneratorStrategy::Finite
                } else if let Some(depth) = susbsubmatches.value_of("approx") {
                    GeneratorStrategy::Approx(depth.parse().unwrap())
                } else {
                    GeneratorStrategy::PushDown
                }
            };
            let filter = {
                if susbsubmatches.is_present("naive-filter")  {
                    FilterStrategy::Naive
                } else {
                    FilterStrategy::Inside
                }
            };
            
            let mut grammar_string = String::new();
            stdin().read_to_string(&mut grammar_string).expect(
                "Could not read from stdin. Be sure to provide the gramar file as input.",
            );
            let grammar: Lcfrs<String, String, LogDomain<f64>> = grammar_string.parse().expect(
                "Could not decode the grammar provided via stdin.",
            );

            bincode::serialize_into(
                &mut write::GzEncoder::new(stdout(), Compression::best()),
                &CSRepresentation::<_, _>::new(grammar, filter, strategy),
                bincode::Infinite
            ).unwrap()
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
            
            let csrep: CSRepresentation<String, String> = bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite).unwrap();
            for sentence in word_strings.lines() {
                let words: Vec<String> = sentence.split_whitespace().map(|s| s.to_string()).collect();

                if params.is_present("debugmode") {
                    csrep.debug(words.as_slice(), beam);
                } else {
                    for derivation in csrep.generate(words.as_slice(), beam).take(k) {
                        print!("{}", to_negra(&derivation.into_iter().map(|(k,v)| (k, v.clone())).collect(), 0));
                    }
                }
            }
        },
        _ => (),
    }
}
