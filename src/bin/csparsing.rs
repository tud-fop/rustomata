extern crate bincode;
use clap::{App, Arg, ArgGroup, ArgMatches, SubCommand};
use flate2::{read, write, Compression};
use log_domain::LogDomain;
use std::{fs::File,
          io::{stdin, stdout, Read}};

use rustomata::grammars::{lcfrs::{cs_representation::{CSRepresentation},
                                  Lcfrs},
                          pmcfg::negra::{to_negra, DumpMode, noparse}};
use rustomata::util::{agenda::Capacity, reverse::Reverse};

pub fn get_sub_command(name: &str) -> App {
    SubCommand::with_name(name)
        .about("Chomsky-Schützenberger representation of LCFRS")
        .subcommand(
            SubCommand::with_name("extract")
                .about(
                    "Reads a grammar from stdin and prints an object
                                       that Chomsky-Schützenberger-represents the grammar.",
                )
                .group(ArgGroup::with_name("strategy"))
                .arg(
                    Arg::with_name("naive")
                        .help("Use a naive Generator automaton.")
                        .long("naive")
                        .group("strategy"),
                )
                .arg(
                    Arg::with_name("approx")
                        .help("Use a regular approximation the push-down generator.")
                        .long("approx")
                        .group("strategy")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("pd")
                        .help("Use a push-down automaton to generate candidates.")
                        .long("pd")
                        .group("strategy"),
                )
                .arg(
                    Arg::with_name("cyk")
                        .help("Evaluate an fsa cyk-style to generate candidates.")
                        .long("cyk")
                        .group("strategy"),
                )
                .group(ArgGroup::with_name("filter"))
                .arg(
                    Arg::with_name("naive-filter")
                        .help("Use a naive filter automaton.")
                        .long("naive-filter")
                        .group("filter"),
                )
                .arg(
                    Arg::with_name("inside-filter")
                        .help("Use the inside filter automaton.")
                        .long("inside-filter")
                        .group("filter"),
                ),
        )
        .subcommand(
            SubCommand::with_name("parse")
                .about(
                    "Reads a list of words from stdin and parses them
                                       using the CS representation of a grammar.",
                )
                .arg(
                    Arg::with_name("csfile")
                        .required(true)
                        .index(1)
                        .help("The file that contains the CS representation of a grammar."),
                )
                .arg(
                    Arg::with_name("k")
                        .short("k")
                        .takes_value(true)
                        .help("Puts the `k` in k-best parsing."),
                )
                .arg(
                    Arg::with_name("beam")
                        .short("b")
                        .long("beam")
                        .takes_value(true)
                        .help("Beam with during search."),
                )
                .arg(
                    Arg::with_name("candidates")
                        .short("c")
                        .long("candidates")
                        .takes_value(true)
                        .help("Maximum number of candidates to enumerate."),
                )
                .arg(
                    Arg::with_name("with-lines")
                        .short("l")
                        .long("with-lines")
                        .help("The first symbol denotes the id of the parse tree."),
                )
                .arg(
                    Arg::with_name("with-pos")
                        .short("p")
                        .long("with-pos")
                        .help("Parses the sentence with already annotated POS tags."),
                )
                .arg(
                    Arg::with_name("debugmode")
                        .short("d")
                        .long("debug")
                        .help("Prints debug information instead of parse trees."),
                )
                .arg(
                    Arg::with_name("fallback")
                        .short("f")
                        .long("with-fallback")
                        .help("Will output an incorrect parse tree if the parse does not find a correct one."),
                ),
        )
}

pub fn handle_sub_matches(submatches: &ArgMatches) {
    match submatches.subcommand() {
        ("extract", Some(_)) => {
            let mut grammar_string = String::new();
            stdin()
                .read_to_string(&mut grammar_string)
                .expect("Could not read from stdin. Be sure to provide the gramar file as input.");
            let grammar: Lcfrs<String, String, Reverse<LogDomain<f64>>> = grammar_string
                .parse()
                .expect("Could not decode the grammar provided via stdin.");

            bincode::serialize_into(
                &mut write::GzEncoder::new(stdout(), Compression::best()),
                &CSRepresentation::new(grammar),
                bincode::Infinite,
            ).unwrap()
        }

        ("parse", Some(params)) => {
            let mut word_strings = String::new();
            stdin()
                .read_to_string(&mut word_strings)
                .expect("Please pass a string to parse using stdin or using the `--word` option.");
            let k: usize = match params.value_of("k") {
                Some(n) => n.parse::<usize>().unwrap(),
                None => 1usize,
            };
            
            let beam: Capacity = match params.value_of("beam") {
                Some(b) => b.parse().unwrap(),
                None => Capacity::Infinite,
            };
            let candidates: Capacity = match params.value_of("candidates") {
                Some(c) => c.parse().unwrap(),
                None => Capacity::Infinite,
            };

            let csfile = File::open(params.value_of("csfile").unwrap()).unwrap();

            let csrep: CSRepresentation<String, String, Reverse<LogDomain<f64>>> =
                bincode::deserialize_from(&mut read::GzDecoder::new(csfile), bincode::Infinite)
                    .unwrap();

            for (i, sentence) in word_strings.lines().enumerate() {
                let (i, words) = split_line(sentence, params.is_present("with-lines"), i);
                let (words, negra_mode) = split_pos(words, params.is_present("with-pos"));

                if params.is_present("debugmode") {
                    let tuple = csrep.debug(words.as_slice(), beam, candidates);
                    eprint!("{} {} {} {} ", tuple.0, tuple.1, tuple.2, tuple.3);
                    if let Some((t, c)) = tuple.4 {
                        eprintln!("{}", c);
                        println!("{}", to_negra(&t, i, negra_mode));
                    } else {
                        eprintln!("{}", -1);
                        println!("{}", noparse(&words, i, negra_mode));
                    }
                } else {
                    let mut found_trees = false;
                    let (iterator, fallback) = csrep.generate(words.as_slice(), beam, candidates);
                    for derivation in iterator.take(k) {
                        found_trees = true;
                        println!(
                            "{}",
                            to_negra(
                                &derivation
                                    .into_iter()
                                    .map(|(k, v)| (k, v.clone()))
                                    .collect(),
                                i,
                                negra_mode.clone()
                            )
                        );
                    }
                    if !found_trees && params.is_present("fallback") {
                        if let Some(tree) = fallback {
                            println!("{}", to_negra(&tree, i, negra_mode));
                        } else {
                            println!("{}", noparse(&words, i, negra_mode));
                        }
                    } else if !found_trees {
                        println!("{}", noparse(&words, i, negra_mode));
                    }
                }
            }
        }
        _ => (),
    }
}

fn split_line<'a>(line: &'a str, with_line_number: bool, default_line_number: usize) -> (usize, impl Iterator<Item=&'a str> + 'a) {
    let mut word_iterator = line.split_whitespace();
    
    let line_number = if with_line_number {
        word_iterator.next()
                     .expect("missing line number")
                     .parse()
                     .expect("invalid line numer")
    } else {
        default_line_number
    };

    (line_number, word_iterator)
}

fn split_pos<'a>(words: impl Iterator<Item=&'a str> + 'a, with_pos: bool) -> (Vec<String>, DumpMode<String>) {
    if with_pos {
        let (pos, ws) = words.map(
            |wp| {
                let mut it = wp.rsplitn(2, '/');
                ( it.next().unwrap().to_string(),
                  it.next().expect("missing pos annotation").to_string()
                )
            }
        ).unzip();
        (pos, DumpMode::FromPos(ws))
    } else {
        (words.map(|s| s.to_string()).collect(), DumpMode::Default)
    }
}