pub mod cli;
pub mod benchmark;

use std::collections::BinaryHeap;
use std::cmp::Ord;
use std::fmt::Debug;
use std::ops::Mul;
use num_traits::One;
use std::fmt;

use recognisable::automaton::Automaton;

use recognisable::*;
use approximation::*;

type Item<S, I, T, W> = (Configuration<S, T, W>, Vec<Transition<I, T, W>>);

/// One level in a coarse-to-fine scheme.
/// Translates a `run` via an `ApproximationStrategy` and checks whether the resulting
/// `Vec`s of `Transition`s are accepted by the given `Automaton`.
pub fn ctf_level<I1, I2, T, W, ST, A>
    (run: Vec<Transition<I2, T, W>>, strat: &ST, automaton: &A) -> BinaryHeap<Item<I1::Storage, I1, T, W>>
    where I1::Storage: Clone + Debug + Eq + Ord,
          I1: Eq + Clone + Debug + Instruction + PartialOrd,
          I2: Eq + Clone + Debug + Instruction,
          T: Eq + Clone + Debug + Ord,
          W: Copy + Ord + Eq + Clone + Debug + Mul<Output = W> + One,
          ST: ApproximationStrategy<I1, I2, T, W>,
          A: Automaton<T, W, I=I1>,
{
    let mut outp = BinaryHeap::new();
    for e in strat.translate_run(run) {
        if let Some(x) = automaton.check_run(&e) {
            outp.push(x);
        }
    }
    outp
}

pub struct Run<A> {
    v: Vec<A>,
}

impl<A> Run<A> {
    pub fn new(v: Vec<A>) -> Run<A> {
        Run { v: v }
    }
}

impl<A: fmt::Display> fmt::Display for Run<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        let mut iter1 = self.v.iter().peekable();

        while let Some(nt) = iter1.next() {
            buffer.push_str(format!("{}", nt).as_str());
            if iter1.peek().is_some() {
                buffer.push_str(",\n ");
            }
        }
        write!(f, "[{}]", buffer)
    }
}

/// returns the word recognised by a `run`
pub fn run_word<I: Instruction, T: Clone, W>(v: &[Transition<I, T, W>]) -> Vec<T> {
    let mut word = Vec::new();
    for t in v {
        let mut t2 = t.word.clone();
        word.append(&mut t2);
    }
    word
}

/// returns the weight of a `run`
pub fn run_weight<I: Instruction, T, W: Mul<Output = W> + Copy + One>(
    v: &[Transition<I, T, W>],
) -> W {
    let mut weight = W::one();
    for t in v {
        weight = weight * t.weight;
    }
    weight
}
