use std::marker::PhantomData;
use std::collections::{BinaryHeap, BTreeMap};
use std::hash::Hash;

use automata::{Transition, TransitionKey};
use approximation::*;
use push_down_automaton::*;

/// `ApproximationStrategy`that limits a `PushDownAutomaton` to a certain height.
#[derive(Clone, Debug)]
pub struct PDTopKElement<A, T1, T2>{
    pub dummy: PhantomData<A>,
    pub trans_map: BTreeMap<T2,Vec<T1>>,
    pub size: usize,
}

impl<A, T1, T2 : Ord> PDTopKElement<A, T1, T2>{
    pub fn new(size: usize) -> PDTopKElement<A, T1, T2> {
        assert!(size >= 1);
        PDTopKElement{
            dummy: PhantomData,
            trans_map: BTreeMap::new(),
            size: size,
        }
    }
}

impl<A, T, W> ApproximationStrategy<PushDown<A>,
                                    PushDown<A>,
                                    Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
                                    Transition<PushDown<A>, PushDownInstruction<A>, T, W>>
    for PDTopKElement<A,
                      Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
                      TransitionKey<PushDown<A>, PushDownInstruction<A>, T, W>>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          T: Ord + Eq + Clone + Hash,
          W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One
{
    fn approximate_initial(&self, a: PushDown<A>) -> PushDown<A> {
        a
    }

    fn approximate_transition(&mut self, t: Transition<PushDown<A>, PushDownInstruction<A>, T, W>) ->
        Transition<PushDown<A>, PushDownInstruction<A>, T, W>{
        match t.instruction{
            PushDownInstruction::Replace { ref current_val, ref new_val }
            | PushDownInstruction::ReplaceK { ref current_val, ref new_val, .. } => {
                let t2 = Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::ReplaceK {
                        current_val: current_val.clone(),
                        new_val: new_val.clone(),
                        limit: self.size,
                    }
                };
                let tk = TransitionKey::new(&t2);
                if !self.trans_map.contains_key(&tk) {
                    self.trans_map.insert(tk.clone(), Vec::new());
                    ()
                }
                self.trans_map.get_mut(&tk).unwrap().push(t.clone());
                t2
            },
        }
    }

    fn translate_run(&self, run: Vec<Transition<PushDown<A>, PushDownInstruction<A>, T, W>>) -> BinaryHeap<PushDownTransitionSequence<A, T, W>>{
        let mut res = Vec::new();
        for lv in run{
            let lvk = TransitionKey::new(&lv);
            match self.trans_map.get(&lvk) {
                Some(v) =>{
                    if res.is_empty() {
                        res.push(v.clone())
                    } else {
                        let mut res2 = Vec::new();
                        for t in v {
                            for r1 in res.clone() {
                                let mut r2 = r1.clone();
                                r2.push(t.clone());
                                res2.push(r2);
                            }
                        }
                        res = res2;
                    }
                },
                None => {
                    return BinaryHeap::new();
                },
            }
        }
        BinaryHeap::from(res)
    }

    fn add_transitions(&mut self, t1: &Transition<PushDown<A>, PushDownInstruction<A>, T, W>, t2: &Transition<PushDown<A>, PushDownInstruction<A>, T, W>){
        let tk = TransitionKey::new(t2);
        if !self.trans_map.contains_key(&tk) {
            self.trans_map.insert(tk.clone(), Vec::new());
            ()
        }
        self.trans_map.get_mut(&tk).unwrap().push(t1.clone());
    }
}
