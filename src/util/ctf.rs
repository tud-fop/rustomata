use std::collections::BinaryHeap;
use std::cmp::Ord;
use std::fmt::Debug;
use std::ops::Mul;
use num_traits::One;
use std::fmt;

pub use automata::*;
pub use approximation::*;

pub fn ctf_level<S1: Eq + Clone + Debug,
                 S2: Eq + Clone + Debug,
                 I1: Eq + Clone + Debug + Instruction<S1>,
                 I2: Eq + Clone + Debug + Instruction<S2>,
                 T1: Eq + Clone + Debug,
                 T2: Eq + Clone + Debug,
                 W:  Copy+ Ord + Eq + Clone + Debug + Mul<Output=W> + One,
                 ST: ApproximationStrategy<S1, S2, Transition<S1, I1, T1, W>, Transition<S2, I2, T2, W>>,
                 A: Automaton<S1, I1, T1, W>
                 >(word : &Vec<T1>, run: Vec<Transition<S2, I2, T2, W>>, strat: &ST, automaton: &A) -> BinaryHeap<(Configuration<S1, T1, W>, Vec<Transition<S1, I1, T1, W>>)>{

    let mut outp = BinaryHeap::new();
    let v = strat.translate_run(run);
    if v.is_empty(){
        return BinaryHeap::new();
    }
    for e in v{
        match automaton.check_run(&e, word.clone()){
            Some((c, e2)) =>{
                outp.push((c,e2));
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
