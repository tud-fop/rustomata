use std::collections::{BinaryHeap, BTreeMap};
use std::hash::Hash;
use std::ops::Add;

use recognisable::Transition;
use approximation::*;
use push_down_automaton::*;

/// `ApproximationStrategy`that limits a `PushDownAutomaton` to a certain height.
#[derive(Clone, Debug)]
pub struct PDTopKElement<T1, T2>{
    pub trans_map: BTreeMap<T2,Vec<T1>>,
    pub size: usize,
}

impl<T1, T2: Ord> PDTopKElement<T1, T2>{
    pub fn new(size: usize) -> PDTopKElement<T1, T2> {
        assert!(size >= 1);
        PDTopKElement{
            trans_map: BTreeMap::new(),
            size: size,
        }
    }
}

impl<A, T, W> ApproximationStrategy<PushDownInstruction<A>, PushDownInstruction<A>, T, W>
    for PDTopKElement<Transition<PushDownInstruction<A>, T, W>,
                      Transition<PushDownInstruction<A>, T, W>>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          T: Ord + Eq + Clone + Hash,
          W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One
{
    fn approximate_initial(&self, a: PushDown<A>) -> PushDown<A> {
        a
    }

    fn approximate_transition(&mut self, t: Transition<PushDownInstruction<A>, T, W>) ->
        Transition<PushDownInstruction<A>, T, W>{
        match t.instruction{
            PushDownInstruction::Replace { ref current_val, ref new_val }
            | PushDownInstruction::ReplaceK { ref current_val, ref new_val, .. } => {
                let t2 = Transition {
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::ReplaceK {
                        current_val: current_val.clone(),
                        new_val: new_val.clone(),
                        limit: self.size,
                    }
                };
                if !self.trans_map.contains_key(&t2) {
                    self.trans_map.insert(t2.clone(), Vec::new());
                    ()
                }
                self.trans_map.get_mut(&t2).unwrap().push(t.clone());
                t2
            },
        }
    }

    fn translate_run(&self, run: Vec<Transition<PushDownInstruction<A>, T, W>>) -> BinaryHeap<PushDownTransitionSequence<A, T, W>>{
        let mut res = Vec::new();
        for lv in run{
            match self.trans_map.get(&lv) {
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

    fn add_transitions(&mut self, t1: &Transition<PushDownInstruction<A>, T, W>, t2: &Transition<PushDownInstruction<A>, T, W>){
        if !self.trans_map.contains_key(&t2) {
            self.trans_map.insert(t2.clone(), Vec::new());
            ()
        }
        self.trans_map.get_mut(&t2).unwrap().push(t1.clone());
    }
}
