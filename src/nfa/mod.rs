use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;
use num_traits::{One};
use std::ops::{Mul};
use std::io::{self,Write};

use integeriser::{Integeriser, HashIntegeriser};

use automata::*;
use push_down::*;

/// Dictonary able to translate a `NFA` back into a `PushDownAutomaton`
#[derive(Debug, PartialEq)]
pub struct Dict<S, I: Instruction<S>, T: Eq + Hash, W: Eq + Ord>{
    map: HashMap<NFATransition<usize, T, W>, Transition<S, I, T, W>>,
}

/// `Transition` equivalent for `NFA`
#[derive(Debug, Clone)]
pub struct NFATransition<S: Eq + Hash, T: Eq + Hash, W: Ord + Eq>{
    from_state: S,
    to_state: S,
    word: Vec<T>,
    weight: W,
}

/// Structure encoding a Automaton without storage (i.e. not a `Automaton`).
pub struct NFA<S: Eq + Hash, T: Eq + Hash, W: Eq + Ord>{
    //states: HashSet<S>,
    transitions: HashMap<S, BinaryHeap<NFATransition<S, T, W>>>,
    initial_states: HashSet<S>,
    final_states: HashSet<S>,
}

impl<S: Eq + Hash + Ord + Clone + Debug, T: Eq + Hash + Clone + Debug, W: Eq + Ord + One + Clone + Debug> NFA<S, T, W>{
    pub fn new(/*states: HashSet<S>,*/ transitions: HashMap<S, BinaryHeap<NFATransition<S, T, W>>>, initial_states: HashSet<S>, final_states: HashSet<S>)-> NFA<S, T, W>{
        NFA{
            //states: states,
            transitions: transitions,
            initial_states: initial_states,
            final_states: final_states,
        }
    }

    pub fn recognise(&self, word: Vec<T>) -> NFARecogniser<S, T, W> {
        let mut init_heap = BinaryHeap::new();
        for i in self.initial_states.clone(){
            let c = Configuration {
                word: word.clone(),
                storage: i.clone(),
                weight: W::one(),
            };
            init_heap.push((c, Vec::new()));
        }
        NFARecogniser {
            agenda: init_heap,
            filtered_rules: self.transitions.clone(),
            accepting: self.final_states.clone(),
            //used: HashSet::new(),
        }
    }
}

impl<S: Eq + Clone + Hash, T: Eq + Clone + Hash, W: Ord + Eq + Clone + Mul<Output=W>> NFATransition<S, T, W>{
    pub fn new(from: S, to: S, word: Vec<T>, weight: W)->Self{
        NFATransition{
            from_state: from,
            to_state: to,
            word: word,
            weight: weight,
        }
    }

    pub fn apply(&self, c: &Configuration<S, T, W>) -> Vec<Configuration<S, T, W>>{
        if !(c.word.starts_with(&self.word[..])) || !(c.storage == self.from_state) {
            return Vec::new()
        }
        let c1 = Configuration{
            word: c.word.clone().split_off(self.word.len()),
            storage: self.to_state.clone(),
            weight: c.weight.clone() * self.weight.clone(),
        };
        vec![c1]
    }
}

impl<S: Eq + Hash, T: Eq + Hash, W: Ord + PartialOrd + Eq> PartialOrd for NFATransition<S, T, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.weight.partial_cmp(&other.weight)
    }
}

impl<S: Eq + Hash, T: Eq + Hash, W: Ord + Eq> Ord for NFATransition<S, T, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.weight.cmp(&other.weight)
    }
}

impl<S: Eq + Hash, T: Eq + Hash, W: Ord + Eq> PartialEq for NFATransition<S, T, W> {
    fn eq(&self, other: &NFATransition<S, T, W>)-> bool{
        (self.from_state == other.from_state) &
        (self.to_state == other.to_state) &
        (self.word == other.word) &
        (self.weight == other.weight)
    }
}

impl<S: Eq + Hash, T: Eq + Hash, W: Ord + Eq> Hash for NFATransition<S, T, W>{
    fn hash<H: Hasher>(&self, state: &mut H){
        self.from_state.hash(state);
        self.to_state.hash(state);
        self.word.hash(state);
    }
}

impl<S: Eq + Hash, T: Eq + Hash, W: Ord + Eq> Eq for NFATransition<S, T, W>{}

impl<S: Clone + Debug, I: Instruction<S> + Clone + Debug, T: Eq + Hash + Clone + Debug, W: Eq + Clone + Ord + Debug> Dict<S, I, T, W>{
    pub fn new(map : HashMap<NFATransition<usize, T, W>, Transition<S, I, T, W>>)->Self{
        Dict{
            map: map,
        }
    }

    pub fn translate(&self, v: Vec<NFATransition<usize, T, W>>)-> Vec<Transition<S, I, T, W>>{
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

/// `Recogniser` equivalent for `NFA`
pub struct NFARecogniser<S: Clone + Ord + Hash + Eq, T: Eq + Hash, W: Eq + Ord> {
    agenda: BinaryHeap<(Configuration<S, T, W>, Vec<NFATransition<S, T, W>>)>,
    filtered_rules: HashMap<S, BinaryHeap<NFATransition<S, T, W>>>,
    accepting: HashSet<S>,
    //used: HashSet<Configuration<S, T, W>>,
}

impl<S: Clone + Ord + Hash + Eq, T: Eq + Hash, W: Eq + Ord> NFARecogniser<S, T, W>{
    fn accepts(&self, c: &Configuration<S, T, W>)-> bool{
        self.accepting.contains(&c.storage) && c.word.is_empty()
    }
}

impl<S: Clone + Ord + Hash + Eq + Debug, T: Eq + Hash + Clone + Debug, W: One + Mul<Output = W> + Clone + Eq + Ord + Debug> Iterator for NFARecogniser<S, T, W> {
    type Item = (Configuration<S, T, W>, Vec<NFATransition<S, T, W>>);

    fn next(&mut self) -> Option<(Configuration<S, T, W>, Vec<NFATransition<S, T, W>>)> {
        let mut i = 0;
        while let Some((c, run)) = self.agenda.pop() {
            i = i + 1;
            //self.used.insert(c.clone());
            for rs in self.filtered_rules.get(&(c.storage)){
                for r in rs {
                    let cv = r.apply(&c);
                    for c1 in cv{
                        //if !self.used.contains(&c1){
                        let mut run1 = run.clone();
                        run1.push(r.clone());
                        self.agenda.push((c1, run1))
                        //}
                    }
                }
            }
            if self.accepts(&c) {
                writeln!(io::stderr(), "New successful configuration found after inspecting {} configurations.", i).unwrap();
                return Some((c, run));
            }
        }
        writeln!(io::stderr(), "No new successful configuration found after inspecting {} configurations.", i).unwrap();
        None
    }
}

/// Creates a `NFA` from a `PushDownAutomaton` including `Dict` to translate it back. Returns `None` when a `Replace` instruction is found
pub fn from_pd<A: PartialEq + Hash + Ord + Clone + Debug,
               T: PartialEq + Eq + Hash + Clone + Debug,
               W: PartialEq + Eq + Clone + Ord + Copy + Mul<Output=W> + Debug + One>(a: &PushDownAutomaton<A, T, W>) -> Option<(NFA<usize, T, W>, Dict<PushDown<A>, PushDownInstruction<A>, T, W>)>{
    let mut integeriser: HashIntegeriser<PushDown<A>> = HashIntegeriser::new();
    let mut map: HashMap<NFATransition<usize, T, W>, Transition<PushDown<A>, PushDownInstruction<A>, T, W>> = HashMap::new();
    let mut to_do = Vec::new();
    let mut states = HashSet::new();
    let mut initial_states = HashSet::new();
    let mut final_states = HashSet::new();

    let mut transitions = HashMap::new();

    initial_states.insert(integeriser.integerise(a.initial().clone()));
    to_do.push(a.initial().clone());

    while let Some(c) = to_do.pop(){
        let ci = integeriser.integerise(c.clone());
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
                                states.insert(c1i);
                                to_do.push(c1);
                            }
                        }
                    }
                }
            }
        }
    }
    Some((NFA::new(/*states,*/ transitions, initial_states, final_states), Dict::new(map)))
}
