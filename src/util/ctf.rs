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

/// One level in the resukting coarse-to-fine scheme. Translates a `run` via a `ApproximationStrategy` and checks wheter the resulting runs are accepted by `Automaton` a.
pub fn ctf_level<S1: Eq + Clone + Debug,
                 S2: Eq + Clone + Debug,
                 I1: Eq + Clone + Debug + Instruction<S1>,
                 I2: Eq + Clone + Debug + Instruction<S2>,
                 T1: Eq + Clone + Debug,
                 T2: Eq + Clone + Debug,
                 W:  Copy+ Ord + Eq + Clone + Debug + Mul<Output=W> + One,
                 ST: ApproximationStrategy<S1, S2, Transition<S1, I1, T1, W>, Transition<S2, I2, T2, W>>,
                 A: Automaton<S1, I1, T1, W>
                 >(run: Vec<Transition<S2, I2, T2, W>>, strat: &ST, automaton: &A) -> BinaryHeap<(Configuration<S1, T1, W>, Vec<Transition<S1, I1, T1, W>>)>{

    let mut outp = BinaryHeap::new();
    let v = strat.translate_run(run);
    if v.is_empty(){
        return BinaryHeap::new();
    }
    for e in v{
        match automaton.check_run(&e){
            Some((c, e2)) =>{
                outp.push((c,e2));
            },
            None =>(),
        }
    }
    return outp;
}

/// Same as `ctf_level`, but for `IntegerisedAutomaton`
pub fn ctf_level_i<'a, S1: Eq + Clone + Debug,
                 S2: Eq + Clone + Debug,
                 I1: Eq + Clone + Debug + Instruction<S1>,
                 I2: Eq + Clone + Debug + Instruction<S2>,
                 T1: Eq + Clone + Debug + Hash,
                 A1: Eq + Clone + Debug + Hash,
                 W:  Copy+ Ord + Eq + Clone + Debug + Mul<Output=W> + One,
                 ST: ApproximationStrategy<S1, S2, Transition<S1, I1, u64, W>, Transition<S2, I2, u64, W>>,
                 A: IntegerisedAutomaton<S1, I1, T1, A1, W>
                 >(run: Vec<Transition<S2, I2, u64, W>>, strat: &ST, automaton: &'a A) -> BinaryHeap<(IntItem<'a, S1, I1, T1, A1, W>)>{

    let mut outp = BinaryHeap::new();
    let v = strat.translate_run(run);
    if v.is_empty(){
        println!("Noooooooooo");
        return BinaryHeap::new();
    }
    for e in v{
        match automaton.check_run(&e){
            Some(x) =>{
                outp.push(x);
            },
            None =>(),
        }
    }
    return outp;
}

pub struct Run<A>{
    v : Vec<A>,
}

impl<A> Run<A>{
    pub fn new(v : Vec<A>)->Run<A>{
        Run{
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

/// rezurns the word recognised by a `run`
pub fn run_word<S, I: Instruction<S>, T: Clone, W>(v: &Vec<Transition<S, I, T, W>>)-> Vec<T>{
    let mut word = Vec::new();
    for t in v{
        let mut t2 = t. word.clone();
        word.append(&mut t2);
    }
    word
}

///returns the weight ammesed by the `run`
pub fn run_weight<S, I: Instruction<S>, T, W: Mul<Output = W> + Copy + One>(v: &Vec<Transition<S, I, T, W>>)-> W{
    let mut weight = W::one();
    for t in v{
        weight = weight * t.weight;
    }
    weight
}
