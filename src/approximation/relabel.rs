use std::marker::PhantomData;
use std::collections::{BinaryHeap, BTreeMap};
use std::hash::Hash;

use automata::{Transition, TransitionKey};
use approximation::*;
pub use approximation::equivalence_classes::EquivalenceClass;
use push_down_automaton::*;

/// Specifies how to relabel the internal values of the structure. Used for `Approximation` with the `RlbElement`
pub trait Relabel<N1, N2, O>{
    fn relabel(&self, &EquivalenceClass<N1, N2>) -> O;
}

/// `ApproximationStrategy` that uses the `Relabel` trait to relabel internal values via a `EquivalenceClass`
#[derive(Clone)]
pub struct RlbElement<A, N1, N2, T1, T2>{
    pub dummy: PhantomData<A>,
    pub trans_map: BTreeMap<T2,Vec<T1>>,
    pub mapping: EquivalenceClass<N1, N2>
}

impl<A, T1, T2 : Ord, N1, N2> RlbElement<A, N1, N2, T1, T2>{
    pub fn new(mapping: EquivalenceClass<N1, N2>) -> RlbElement<A, N1, N2, T1, T2>{
        RlbElement{
            dummy : PhantomData,
            trans_map : BTreeMap::new(),
            mapping : mapping,
        }
    }
}

impl <A1: Ord + PartialEq + Debug + Clone + Hash + Relabel<N1, N2, A2>,
      A2:  Ord + PartialEq + Debug + Clone + Hash,
      N1: Clone + Eq + Hash,
      N2: Clone + Eq + Hash,
      T: Ord + Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One> ApproximationStrategy<PushDownInstruction<A1>, PushDownInstruction<A2>, T, W>
      for RlbElement<PushDown<A1>, N1, N2, Transition<PushDownInstruction<A1>, T, W>, TransitionKey<PushDownInstruction<A2>, T, W>>{
    fn approximate_initial(&self, a: PushDown<A1>)-> PushDown<A2>{
        a.relabel(&self.mapping)
    }

    fn approximate_transition(&mut self, t:  Transition<PushDownInstruction<A1>, T, W>) ->
        Transition<PushDownInstruction<A2>, T, W> {
        match t.instruction {
            PushDownInstruction::Replace {ref current_val, ref new_val} => {
                let mut stc = Vec::new();
                let mut stn = Vec::new();
                for nt in current_val {
                    stc.push(nt.relabel(&self.mapping));
                }
                for nt in new_val {
                    stn.push(nt.relabel(&self.mapping));
                }
                let t2 = Transition {
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: stc.clone(),
                        new_val: stn.clone(),
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
            PushDownInstruction::ReplaceK {ref current_val, ref new_val, limit} => {
                let mut stc = Vec::new();
                let mut stn = Vec::new();
                for nt in current_val {
                    stc.push(nt.relabel(&self.mapping));
                }
                for nt in new_val {
                    stn.push(nt.relabel(&self.mapping));
                }
                let t2 = Transition {
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::ReplaceK {
                        current_val: stc.clone(),
                        new_val: stn.clone(),
                        limit: limit,
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

    fn translate_run(&self, run: Vec<Transition<PushDownInstruction<A2>, T, W>>) -> BinaryHeap<PushDownTransitionSequence<A1, T, W>>{
        let mut res = Vec::new();
        for lv in run {
            let lvk = TransitionKey::new(&lv);
            match self.trans_map.get(&lvk) {
                Some(v) => {
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
                None => return BinaryHeap::new(),
            }
        }
        BinaryHeap::from(res)
    }

    fn add_transitions(&mut self, t1: &Transition<PushDownInstruction<A1>, T, W>, t2: &Transition<PushDownInstruction<A2>, T, W>){
        let tk = TransitionKey::new(t2);
        if !self.trans_map.contains_key(&tk) {
            self.trans_map.insert(tk.clone(), Vec::new());
            ()
        }
        self.trans_map.get_mut(&tk).unwrap().push(t1.clone());
    }
}

// needed for integerised values
impl Relabel<usize, usize, usize> for usize {
    fn relabel(&self, map: &EquivalenceClass<usize, usize>) -> usize {
        *map.project(self)
    }
}
