use clap::{App, Arg, ArgMatches, SubCommand};
use log_domain::LogDomain;
use rustomata::approximation::equivalence_classes::EquivalenceRelation;
use rustomata::approximation::relabel::RlbElement;
use rustomata::approximation::tts::TTSElement;
use rustomata::approximation::ApproximationStrategy;
use rustomata::automata::push_down_automaton::{PushDownAutomaton, PushState};
use rustomata::automata::tree_stack_automaton::TreeStackAutomaton;
use rustomata::grammars::cfg::CFG;
use rustomata::grammars::pmcfg::PMCFG;
use rustomata::recognisable::Recognisable;

use std::fs::File;
use std::io::{self, Read};

pub fn get_sub_command() -> App<'static, 'static> {
    SubCommand::with_name("approximation")
        .author("Max Korn <max.korn@tu-dresden.de>")
        .about("functions related to single approximations")
        .subcommand(
            SubCommand::with_name("relabel")
                .about("relabels automata with classes file")
                .subcommand(
                    SubCommand::with_name("parse")
                        .arg(
                            Arg::with_name("classes")
                                .help("classes file to use")
                                .index(1)
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("grammar")
                                .help("cfg-grammar file to use")
                                .index(2)
                                .required(true),
                        ),
                )
                .subcommand(
                    SubCommand::with_name("automaton")
                        .arg(
                            Arg::with_name("classes")
                                .help("classes file to use")
                                .index(1)
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("grammar")
                                .help("cfg-grammar file to use")
                                .index(2)
                                .required(true),
                        ),
                ),
        )
        .subcommand(
            SubCommand::with_name("topk")
                .about("maps pushdown to its topmost k elements")
                .subcommand(
                    SubCommand::with_name("parse")
                        .arg(
                            Arg::with_name("grammar")
                                .help("cfg-grammar file to use")
                                .index(1)
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("size")
                                .help("size of pushdown")
                                .index(2)
                                .required(true),
                        ),
                )
                .subcommand(
                    SubCommand::with_name("automaton")
                        .arg(
                            Arg::with_name("grammar")
                                .help("cfg-grammar file to use")
                                .index(1)
                                .required(true),
                        )
                        .arg(
                            Arg::with_name("size")
                                .help("size of pushdown")
                                .index(2)
                                .required(true),
                        ),
                ),
        )
        .subcommand(
            SubCommand::with_name("tts")
                .about("approximates tree-stack into pushdown automata")
                .subcommand(
                    SubCommand::with_name("parse").arg(
                        Arg::with_name("grammar")
                            .help("mcfg-grammar file to use")
                            .index(1)
                            .required(true),
                    ),
                )
                .subcommand(
                    SubCommand::with_name("automaton").arg(
                        Arg::with_name("grammar")
                            .help("mcfg-grammar file to use")
                            .index(1)
                            .required(true),
                    ),
                ),
        )
}

pub fn handle_sub_matches(r_matches: &ArgMatches) {
    match r_matches.subcommand() {
        ("relabel", Some(relabel_matches)) => match relabel_matches.subcommand() {
            ("parse", Some(parse_matches)) => {
                let grammar_file_name = parse_matches.value_of("grammar").unwrap();
                let mut grammar_file = File::open(grammar_file_name).unwrap();
                let mut grammar_string = String::new();
                let _ = grammar_file.read_to_string(&mut grammar_string);
                let g: CFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();

                let a = PushDownAutomaton::from(g);

                let classes_file_name = parse_matches.value_of("classes").unwrap();
                let mut classes_file = File::open(classes_file_name).unwrap();
                let mut classes_string = String::new();
                let _ = classes_file.read_to_string(&mut classes_string);
                let e: EquivalenceRelation<String, String> = classes_string.parse().unwrap();

                let f = |ps: &PushState<_, _>| ps.map(|nt| e.project(nt));
                let rlb = RlbElement::new(&f);

                let (b, _) = rlb.approximate_automaton(&a);

                let mut corpus = String::new();
                let _ = io::stdin().read_to_string(&mut corpus);

                for sentence in corpus.lines() {
                    println!(
                        "{:?}: {}",
                        b.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect(),)
                            .next(),
                        sentence
                    );
                }
            }
            ("automaton", Some(parse_matches)) => {
                let grammar_file_name = parse_matches.value_of("grammar").unwrap();
                let mut grammar_file = File::open(grammar_file_name).unwrap();
                let mut grammar_string = String::new();
                let _ = grammar_file.read_to_string(&mut grammar_string);
                let g: CFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();

                let a = PushDownAutomaton::from(g);

                let classes_file_name = parse_matches.value_of("classes").unwrap();
                let mut classes_file = File::open(classes_file_name).unwrap();
                let mut classes_string = String::new();
                let _ = classes_file.read_to_string(&mut classes_string);
                let e: EquivalenceRelation<String, String> = classes_string.parse().unwrap();

                let f = |ps: &PushState<_, _>| ps.map(|nt| e.project(nt));
                let rlb = RlbElement::new(&f);

                let (b, _) = rlb.approximate_automaton(&a);

                println!("{}", b);
            }
            _ => (),
        },
        ("tts", Some(tts_matches)) => match tts_matches.subcommand() {
            ("parse", Some(parse_matches)) => {
                let grammar_file_name = parse_matches.value_of("grammar").unwrap();
                let mut grammar_file = File::open(grammar_file_name).unwrap();
                let mut grammar_string = String::new();
                let _ = grammar_file.read_to_string(&mut grammar_string);
                let g: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();

                let a = TreeStackAutomaton::from(g);

                let tts = TTSElement::new();

                let (b, _) = tts.approximate_automaton(&a);

                let mut corpus = String::new();
                let _ = io::stdin().read_to_string(&mut corpus);

                for sentence in corpus.lines() {
                    println!(
                        "{:?}: {}",
                        b.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect(),)
                            .next(),
                        sentence
                    );
                }
            }
            ("automaton", Some(parse_matches)) => {
                let grammar_file_name = parse_matches.value_of("grammar").unwrap();
                let mut grammar_file = File::open(grammar_file_name).unwrap();
                let mut grammar_string = String::new();
                let _ = grammar_file.read_to_string(&mut grammar_string);
                let g: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();

                let a = TreeStackAutomaton::from(g);
                let tts = TTSElement::new();

                let (b, _) = tts.approximate_automaton(&a);
                println!("{}", b);
            }
            _ => (),
        },
        _ => (),
    }
}
