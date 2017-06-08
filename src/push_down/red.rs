use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Add, Mul, Div};
use self::num_traits::{One, Zero};

use automata;

pub use automata::red::*;
pub use util::*;
pub use push_down::*;

impl<A: Ord + PartialEq + Debug + Clone + Hash,
     T: Eq + Clone + Hash,
     W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output = W> + Add<f64, Output = f64> + Zero + One> Redundancy for PushDownAutomaton<A, T, W>{
    fn reduce_redundancy(self)-> PushDownAutomaton<A, T, W>{
        let tm = self.transitions.clone();
        let mut transition_map : HashMap<A, BinaryHeap<automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>>  = HashMap::new();
        //for every configuration
        for (k,v) in tm{
            //samples Transitions into Key(Instruction, Word), Value(Vec<Weight>) pairs.
            let mut c_map : HashMap<TransitionKey<PushDownInstruction<A>, T>,Vec<W>>  = HashMap::new();
            for t in v{
                let tkey = TransitionKey{
                    instruction: t.instruction.clone(),
                    word : t.word.clone(),
                };
                if !c_map.contains_key(&tkey) {
                    c_map.insert(tkey.clone(), Vec::new());
                }
                c_map.get_mut(&tkey).unwrap().push(t.weight.clone());
            }
            //gets the number of redundant transitions (not quite secure jet)
            let mut sum_of_weights = 0.0 as f64;
            for (_, tv) in c_map.clone(){
                for tvv in tv{
                    sum_of_weights = tvv + sum_of_weights;
                }
            }
            //maps the new transitions. Weights are normalized by norm
            let norm = W::one().div(sum_of_weights);
            for (tk, tv) in c_map{
                let mut nw = W::zero();
                for w in tv.clone(){
                    println!("add fault");
                    nw = nw + (w*norm.clone());
                }
                let new_t = automata::Transition{
                    _dummy : PhantomData,
                    word : tk.word.clone(),
                    weight : nw.clone(),
                    instruction : tk.instruction.clone(),
                };
                if !transition_map.contains_key(&k) {
                    transition_map.insert(k.clone(), BinaryHeap::new());
                }
                transition_map.get_mut(&k).unwrap().push(new_t.clone());

            }
        }
        PushDownAutomaton{
            initial : self.initial.clone(),
            transitions: transition_map.clone(),
        }
    }
}
