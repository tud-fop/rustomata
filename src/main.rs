mod automata;
mod tree_stack;

extern crate ordered_float;

#[cfg(test)]
mod tests;

use std::env;
use std::io::prelude::*;
use std::fs::File;
use ordered_float::OrderedFloat;

pub use tree_stack::*;
pub use automata::*;

fn main() {
    let automaton_file_name = env::args_os().nth(1).unwrap();
    let mut automaton_file: File = File::open(automaton_file_name.clone()).unwrap();
    let mut automaton_string = String::new();
    let _ = automaton_file.read_to_string(&mut automaton_string);
    let automaton: TreeStackAutomaton<String, String, OrderedFloat<f64>>
        = automaton_string.parse().unwrap();

    let mut corpus = String::new();
    let _ = std::io::stdin().read_to_string(&mut corpus);

    for sentence in corpus.lines() {
        println!(
            "{:?}: {}",
            automaton.recognise(sentence.split_whitespace().map(|x| x.to_string()).collect(), OrderedFloat(0f64)),
            sentence
        )
    }
}
