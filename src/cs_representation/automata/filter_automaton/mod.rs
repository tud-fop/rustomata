use integeriser::HashIntegeriser;
use pmcfg::PMCFGRule;
use std::hash::Hash;
use cs_representation::bracket_fragment::BracketFragment;

pub mod naive;
pub mod inside;
use super::{FiniteAutomaton, GeneratorAutomaton};

pub use self::naive::NaiveFilterAutomaton;
pub use self::inside::InsideFilterAutomaton;

pub trait FilterAutomaton<T>
where
    T: Hash + Eq + Clone,
{
    fn new<N, W>(
        grammar: &HashIntegeriser<PMCFGRule<N, T, W>>,
        reference_automaton: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> Self
    where
        N: Hash + Eq + Clone,
        W: Eq + Clone;

    fn fsa(
        &self,
        word: &[T],
        reference_automaton: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> FiniteAutomaton<BracketFragment<T>>;
}

fn vec_split<T>(mut xs: Vec<T>) -> (Option<T>, Vec<T>) {
    if xs.is_empty() {
        (None, xs)
    } else {
        let x = xs.remove(0);
        (Some(x), xs)
    }
}

use std::collections::HashMap;
fn get_brackets_with<'a, 'b, T>(
    current_word: &'a [T],
    brackets_with: &'b HashMap<T, Vec<(Vec<T>, usize)>>,
) -> Vec<(&'a [T], usize, usize)>
where
    T: Hash + Eq,
{
    if current_word.is_empty() || !brackets_with.contains_key(&current_word[0]) {
        Vec::new()
    } else {
        let mut results = Vec::new();
        for &(ref following_terminals, brackets) in brackets_with.get(&current_word[0]).unwrap() {
            if following_terminals.is_empty()
                || following_terminals.as_slice() == &current_word[1..(following_terminals.len())]
            {
                results.push((
                    &current_word[(following_terminals.len() + 1)..],
                    brackets,
                    following_terminals.len() + 1,
                ));
            }
        }
        results
    }
}