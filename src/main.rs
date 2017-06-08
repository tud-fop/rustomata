extern crate clap;
#[macro_use]
extern crate nom;
extern crate num_traits;

mod automata;
mod tree_stack;
mod pmcfg;
mod util;

mod push_down;
mod cfg;
mod approximation;

#[cfg(test)]
mod tests;

use clap::{Arg, App, SubCommand};
use std::io::prelude::*;
use std::fs::File;
use std::marker::PhantomData;

pub use tree_stack::*;
pub use automata::*;
pub use pmcfg::*;
pub use util::*;

pub use push_down::*;
pub use cfg::*;
pub use approximation::*;


fn test_equivalence(a : String)->String{
    match &*a{
        "A" => "C".to_string() ,
        "B" => "C".to_string() ,
        _ => a.clone(),
    }
}

fn main() {
    let matches
        = App::new("rustomata")
        .version("0.1")
        .author("Tobias Denkinger <tobias.denkinger@tu-dresden.de>")
        .about("Framework for (weighted) automata with storage")
        .subcommand(SubCommand::with_name("mcfg")
                    .about("functions related to multiple context-free grammars")
                    .subcommand(SubCommand::with_name("parse")
                                .about("parses from stdin with a multiple context-free grammar")
                                .arg(Arg::with_name("grammar")
                                     .help("grammar file to use")
                                     .index(1)
                                     .required(true)))
                    .subcommand(SubCommand::with_name("automaton")
                                .about("constructs a tree-stack automaton from the given multiple context-free grammar")
                                .arg(Arg::with_name("grammar")
                                     .help("grammar file to use")
                                     .index(1)
                                     .required(true))))
        .subcommand(SubCommand::with_name("cfg")
                    .about("functions related to context-free grammars")
                    .subcommand(SubCommand::with_name("parse")
                                .about("parses a word given a multiple context-free grammar")
                                .arg(Arg::with_name("grammar")
                                        .help("grammar file to use")
                                        .index(1)
                                        .required(true)))
                    .subcommand(SubCommand::with_name("automaton")
                                .about("constructs a tree-stack automaton from the given multiple context-free grammar")
                                .arg(Arg::with_name("grammar")
                                        .help("grammar file to use")
                                        .index(1)
                                        .required(true))))
        .subcommand(SubCommand::with_name("tsa")
                    .about("functions related to tree-stack automata")
                    .subcommand(SubCommand::with_name("recognise")
                                .about("recognises from stdin with a tree-stack automaton")
                                .arg(Arg::with_name("automaton")
                                        .help("automaton file to use")
                                        .index(1)
                                        .required(true))))
        .subcommand(SubCommand::with_name("ctf")
                    .about("coarse to fine related functions")
                    .subcommand(SubCommand::with_name("relabelp")
                                .about("relabels automata with testfunction")
                                .arg(Arg::with_name("grammar")
                                        .help("grammar file to use")
                                        .index(1)
                                        .required(true)))
                    .subcommand(SubCommand::with_name("topk")
                                .about("maps pushdown to its topmost k elements")
                                .subcommand(SubCommand::with_name("parse")
                                        .arg(Arg::with_name("grammar")
                                                .help("grammar file to use")
                                                .index(2)
                                                .required(true))
                                        .arg(Arg::with_name("size")
                                                .help("size of pushdown")
                                                .index(1)
                                                .required(true)))
                                .subcommand(SubCommand::with_name("automaton")
                                        .arg(Arg::with_name("grammar")
                                                .help("grammar file to use")
                                                .index(2)
                                                .required(true))
                                        .arg(Arg::with_name("size")
                                                .help("size of pushdown")
                                                .index(1)
                                                .required(true)))))
        .get_matches();

    match matches.subcommand() {
        ("mcfg", Some(mcfg_matches)) => {
            match mcfg_matches.subcommand() {
                ("parse", Some(mcfg_parse_matches)) => {
                    let grammar_file_name = mcfg_parse_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let grammar: PMCFG<String, String, util::log_prob::LogProb> = grammar_string.parse().unwrap();

                    let automaton = TreeStackAutomaton::from(grammar);

                    let mut corpus = String::new();
                    let _ = std::io::stdin().read_to_string(&mut corpus);

                    for sentence in corpus.lines() {
                        println!("{:?}: {}",
                                 automaton.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect()),
                                 sentence);
                    }
                },
                ("automaton", Some(mcfg_automaton_matches)) => {
                    let grammar_file_name = mcfg_automaton_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let grammar: PMCFG<String, String, util::log_prob::LogProb> = grammar_string.parse().unwrap();

                    let automaton = TreeStackAutomaton::from(grammar);
                    println!("{}", automaton);
                }
                _ => ()
            }
        },
        ("cfg", Some(cfg_matches)) => {
            match cfg_matches.subcommand() {
                ("parse", Some(cfg_parse_matches)) => {
                    let grammar_file_name = cfg_parse_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name.clone()).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let grammar: CFG<String, String, util::log_prob::LogProb> = grammar_string.parse().unwrap();

                    let automaton = PushDownAutomaton::from(grammar);

                    let mut corpus = String::new();
                    let _ = std::io::stdin().read_to_string(&mut corpus);

                    for sentence in corpus.lines() {
                        println!("{:?}: {}",
                                 automaton.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect()),
                                 sentence);
                    }
                },
                ("automaton", Some(cfg_automaton_matches)) => {
                    let grammar_file_name = cfg_automaton_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name.clone()).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let grammar: CFG<String, String, util::log_prob::LogProb> = grammar_string.parse().unwrap();

                    let automaton = PushDownAutomaton::from(grammar);
                    println!("{}", automaton);
                },
                _ => ()
            }
        }
        ("tsa", Some(tsa_matches)) => {
            match tsa_matches.subcommand() {
                ("recognise", Some(tsa_recognise_matches)) => {
                    let automaton_file_name = tsa_recognise_matches.value_of("automaton").unwrap();
                    let mut automaton_file = File::open(automaton_file_name).unwrap();
                    let mut automaton_string = String::new();
                    let _ = automaton_file.read_to_string(&mut automaton_string);
                    let automaton: TreeStackAutomaton<String, String, util::log_prob::LogProb> = automaton_string.parse().unwrap();

                    let mut corpus = String::new();
                    let _ = std::io::stdin().read_to_string(&mut corpus);

                    for sentence in corpus.lines() {
                        println!("{:?}: {}",
                                 automaton.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect()),
                                 sentence);
                    }
                },
                _ => ()
            }
        },
        ("ctf", Some(r_matches)) => {
            match r_matches.subcommand() {
                ("relabelp",Some(test_matches)) => {
                    let grammar_file_name = test_matches.value_of("grammar").unwrap();
                    let mut grammar_file = File::open(grammar_file_name.clone()).unwrap();
                    let mut grammar_string = String::new();
                    let _ = grammar_file.read_to_string(&mut grammar_string);
                    let g: CFG<String, String, util::log_prob::LogProb> = grammar_string.parse().unwrap();

                    let a = PushDownAutomaton::from(g);

                    println!("{}", a);

                    let rlb = RlbElement{
                        dummy : PhantomData,
                        func : test_equivalence,
                    };

                    let b = a.approximation(rlb);

                    println!("{}", b.unwrap());
                },
                ("topk",Some(topk_matches)) => {
                    match topk_matches.subcommand(){
                        ("parse",Some(parse_matches)) => {
                            let grammar_file_name = parse_matches.value_of("grammar").unwrap();
                            let mut grammar_file = File::open(grammar_file_name.clone()).unwrap();
                            let mut grammar_string = String::new();
                            let _ = grammar_file.read_to_string(&mut grammar_string);
                            let g: CFG<String, String, util::log_prob::LogProb> = grammar_string.parse().unwrap();

                            let size = parse_matches.value_of("size").unwrap().parse::<usize>().unwrap();

                            let a = PushDownAutomaton::from(g);

                            let ptk = PDTopKElement{
                                dummy : PhantomData,
                                size : size,
                            };
                            let b = a.approximation(ptk).unwrap();

                            let mut corpus = String::new();
                            let _ = std::io::stdin().read_to_string(&mut corpus);

                            for sentence in corpus.lines() {
                                println!("{:?}: {}",
                                         b.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect()),
                                         sentence);
                            }
                        },
                        ("automaton",Some(parse_matches)) => {
                            let grammar_file_name = parse_matches.value_of("grammar").unwrap();
                            let mut grammar_file = File::open(grammar_file_name.clone()).unwrap();
                            let mut grammar_string = String::new();
                            let _ = grammar_file.read_to_string(&mut grammar_string);
                            let g: CFG<String, String, util::log_prob::LogProb> = grammar_string.parse().unwrap();

                            let size = parse_matches.value_of("size").unwrap().parse::<usize>().unwrap();

                            let a = PushDownAutomaton::from(g);

                            println!("Original Automaton");
                            println!("{}", a);

                            let ptk = PDTopKElement{
                                dummy : PhantomData,
                                size : size,
                            };

                            let b = a.approximation(ptk);
                            println!("Approximated Automaton");
                            println!("{}", b.unwrap());
                        },
                        _ => ()
                    }

                },
                _ => ()
            }
        },
        _ => ()
    }

}
