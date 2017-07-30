use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::cmp::Ordering;
use num_traits::{One};
use std::ops::{Mul};

pub use automata::*;
pub use push_down::*;

pub struct Dict<S, I: Instruction<S>, T, W>{
    map: HashMap<NFATransition<u64, T, W>, Transition<S, I, T, W>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NFATransition<S, T, W>{
    from_state: S,
    to_state: S,
    word: Vec<T>,
    weight: W,
}

pub struct NFA<S, T, W>{
    states: HashSet<S>,
    transitions: HashMap<S, BinaryHeap<NFATransition<S, T, W>>>,
    initial_states: HashSet<S>,
    final_states: HashSet<S>,
}

impl<S, T, W> NFA<S, T, W>{
    pub fn new(states: HashSet<S>, transitions: HashMap<S, BinaryHeap< NFATransition<S, T, W>>>, initial_states: HashSet<S>, final_states: HashSet<S>)-> NFA<S, T, W>{
        NFA{
            states: states,
            transitions: transitions,
            initial_states: initial_states,
            final_states: final_states,
        }
    }

    
}

impl<S, T, W> NFATransition<S, T, W>{
    pub fn new(from: S, to: S, word: Vec<T>, weight: W)->Self{
        NFATransition{
            from_state: from,
            to_state: to,
            word: word,
            weight: weight,
        }
    }
}

impl<S: Eq, T: Eq, W: PartialOrd + Eq> PartialOrd for NFATransition<S, T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.weight.partial_cmp(&other.weight)
    }
}

impl<S: Eq, T: Eq, W: Ord + Eq> Ord for NFATransition<S, T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.weight.cmp(&other.weight)
    }
}

impl<S: Clone, I: Instruction<S> + Clone, T: Eq + Hash + Clone, W: Eq + Hash + Clone> Dict<S, I, T, W>{
    pub fn new(map : HashMap<NFATransition<u64, T, W>, Transition<S, I, T, W>>)->Self{
        Dict{
            map: map,
        }
    }

    pub fn translate(&self, v: Vec<NFATransition<u64, T, W>>)-> Vec<Transition<S, I, T, W>>{
        let mut outv = Vec::new();
        for t in v{
            match self.map.get(&t){
                Some(t2) => {
                    outv.push(t2.clone());
                },
                None => {
                    return Vec::new();
                },
            }
        }
        outv
    }
}

///Creates a NFA from a PushDownAutomaton including Dict to translate it back. Returns `None` when a Replace instruction is found
pub fn from_pd<A: PartialEq + Hash + Ord + Clone + Debug,
               T: PartialEq + Eq + Hash + Clone + Debug,
               W: PartialEq + Eq + Hash + Clone + Ord + Copy + Mul<Output=W> + Debug + One>(a: PushDownAutomaton<A, T, W>)->Option<(NFA<u64, T, W>, Dict<PushDown<A>, PushDownInstruction<A>, T, W>)>{
    let mut integeriser : Integeriser<PushDown<A>> = Integeriser::new();
    let mut map : HashMap<NFATransition<u64, T, W>, Transition<PushDown<A>, PushDownInstruction<A>, T, W>> = HashMap::new();
    let mut to_do = Vec::new();
    let mut states = HashSet::new();
    let mut initial_states = HashSet::new();
    let mut final_states = HashSet::new();

    let mut transitions = HashMap::new();

    initial_states.insert(integeriser.integerise(a.initial().clone()));
    to_do.push(a.initial().clone());

    while let Some(c) = to_do.pop(){
        let ci : u64 = integeriser.integerise(c.clone());
        states.insert(ci);
        if c.is_bottom(){
            final_states.insert(ci);
        }
        for rs in a.transitions().get(c.current_symbol()){
            for r in rs{
                match r.instruction{
                    PushDownInstruction::Replace {..} => {
                        return None;
                    }
                    PushDownInstruction::ReplaceK {..} => {
                        for c1 in r.instruction.apply(c.clone()){
                            let c1i = integeriser.integerise(c1.clone());
                            if !transitions.contains_key(&ci) {
                                transitions.insert(ci.clone(), BinaryHeap::new());
                            }
                            let t = NFATransition::new(ci, c1i, r.word.clone(), r.weight.clone());
                            transitions.get_mut(&ci).unwrap().push(t.clone());
                            map.insert(t, r.clone());
                            if !states.contains(&c1i){
                                to_do.push(c1);
                            }
                        }
                    }
                }
            }
        }
    }
    Some((NFA::new(states, transitions, initial_states, final_states),Dict::new(map)))
}
