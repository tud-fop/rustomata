extern crate num_traits;

mod automata;
mod tree_stack;
mod pmcfg;
mod log_prob;
mod util;

#[cfg(test)]
mod tests;

use std::env;
use std::io::prelude::*;
use std::fs::File;

pub use tree_stack::*;
pub use automata::*;
pub use pmcfg::*;
pub use log_prob::*;
pub use util::*;

fn main() {
    let grammar_file_name = env::args_os().nth(1).unwrap();
    let mut grammar_file = File::open(grammar_file_name.clone()).unwrap();
    let mut grammar_string = String::new();
    let _ = grammar_file.read_to_string(&mut grammar_string);
    let grammar: PMCFG<String, String, LogProb> = grammar_string.parse().unwrap();

//    println!("{:?}", grammar);

    let automaton = TreeStackAutomaton::from(grammar);

    let mut corpus = String::new();
    let _ = std::io::stdin().read_to_string(&mut corpus);

    for sentence in corpus.lines() {
        println!("{:?}: {}",
                 automaton.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect()),
                 sentence);
    }
}
