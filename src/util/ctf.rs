use std::collections::BinaryHeap;
use std::cmp::Ord;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Mul;
use num_traits::One;
use std::fmt;

use automata::*;
use approximation::*;
use integerise::*;

type Item<S1, I1, T1, W> = (Configuration<S1, T1, W>, Vec<Transition<S1, I1, T1, W>>);

/// One level in a coarse-to-fine scheme.
/// Translates a `run` via an `ApproximationStrategy` and checks whether the resulting `Vec`s of `Transition`s are accepted by the given `Automaton`.
pub fn ctf_level<S1: Eq + Clone + Debug,
                 S2: Eq + Clone + Debug,
                 I1: Eq + Clone + Debug + Instruction<S1>,
                 I2: Eq + Clone + Debug + Instruction<S2>,
                 T1: Eq + Clone + Debug,
                 T2: Eq + Clone + Debug,
                 W:  Copy+ Ord + Eq + Clone + Debug + Mul<Output=W> + One,
                 ST: ApproximationStrategy<S1, S2, Transition<S1, I1, T1, W>, Transition<S2, I2, T2, W>>,
                 A: Automaton<S1, I1, T1, W>
                 >(run: Vec<Transition<S2, I2, T2, W>>, strat: &ST, automaton: &A) -> BinaryHeap<Item<S1, I1, T1, W>>
{
    let mut outp = BinaryHeap::new();
    for e in strat.translate_run(run) {
        if let Some(x) = automaton.check_run(&e) {
            outp.push(x);
        }
    }
    outp
}

/// Same as `ctf_level`, but for `IntegerisedAutomaton`
pub fn ctf_level_i<'a, S1: Eq + Clone + Debug,
                   S2: Eq + Clone + Debug,
                   I1: Eq + Clone + Debug + Instruction<S1>,
                   I2: Eq + Clone + Debug + Instruction<S2>,
                   T1: Eq + Clone + Debug + Hash,
                   A1: Eq + Clone + Debug + Hash,
                   W:  Copy+ Ord + Eq + Clone + Debug + Mul<Output=W> + One,
                   ST: ApproximationStrategy<S1, S2, Transition<S1, I1, usize, W>, Transition<S2, I2, usize, W>>,
                   A: IntegerisedAutomaton<S1, I1, T1, A1, W>
                   >(run: Vec<Transition<S2, I2, usize, W>>, strat: &ST, automaton: &'a A) -> BinaryHeap<(IntItem<'a, S1, I1, T1, A1, W>)>
{
    let mut outp = BinaryHeap::new();
    for e in strat.translate_run(run) {
        if let Some(x) = automaton.check_run(&e) {
            outp.push(x);
        }
    }
    outp
}

pub struct Run<A>{
    v: Vec<A>,
}

impl<A> Run<A>{
    pub fn new(v: Vec<A>) -> Run<A>{
        Run {
            v: v,
        }
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
pub fn run_word<S, I: Instruction<S>, T: Clone, W>(v: &[Transition<S, I, T, W>]) -> Vec<T> {
    let mut word = Vec::new();
    for t in v{
        let mut t2 = t. word.clone();
        word.append(&mut t2);
    }
    word
}

/// returns the weight of a `run`
pub fn run_weight<S, I: Instruction<S>, T, W: Mul<Output = W> + Copy + One>(v: &[Transition<S, I, T, W>]) -> W {
    let mut weight = W::one();
    for t in v{
        weight = weight * t.weight;
    }
    weight
}
