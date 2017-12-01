use std::fmt::Debug;
use std::ops::{Add, Mul};
use self::num_traits::{One, Zero};

use recognisable::red::*;
use push_down_automaton::*;

type TransitionKeyMap<A, T, W> = HashMap<TransitionKey<PushDownInstruction<A>, T, W>, Vec<W>>;

impl<A: Ord + PartialEq + Debug + Clone + Hash,
     T: Eq + Clone + Hash,
     W: Ord + Eq + Clone + Add + Mul + Zero + One> Redundancy for PushDownAutomaton<A, T, W>{
    fn reduce_redundancy(self)-> PushDownAutomaton<A, T, W>{
        let tm = self.transitions.clone();
        let mut transition_map: TransitionMap<A, T, W> = HashMap::new();
        // for every configuration
        for (k,v) in tm {
            // samples Transitions into Key(Instruction, Word), Value(Vec<Weight>) pairs.
            let mut c_map: TransitionKeyMap<A, T, W> = HashMap::new();
            for t in v{
                let tkey = TransitionKey::new(&t);
                if !c_map.contains_key(&tkey) {
                    c_map.insert(tkey.clone(), Vec::new());
                }
                c_map.get_mut(&tkey).unwrap().push(t.weight.clone());
            }
            // maps the new transitions.
            for (tk, tv) in c_map{
                let mut nw = W::one();
                for w in tv.clone(){
                    nw = nw * w;
                }
                let new_t = Transition {
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
