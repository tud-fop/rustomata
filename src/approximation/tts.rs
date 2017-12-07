use std::collections::{BinaryHeap, BTreeMap};
use std::hash::Hash;
use std::ops::Add;

use recognisable::Transition;
use approximation::*;
use tree_stack_automaton::*;
use push_down_automaton::*;

/// `ÀpproximationStrategy` that approximates a `TreeStackAutomaton` into a `PushDownAutomaton`
#[derive(Clone, Debug)]
pub struct TTSElement<T1, T2>{
    pub trans_map: BTreeMap<T2,Vec<T1>>,
}

impl<T1, T2: Ord> TTSElement<T1, T2> {
    pub fn new() -> TTSElement<T1, T2> {
        TTSElement{
            trans_map: BTreeMap::new(),
        }
    }
}

impl<T1, T2: Ord> Default for TTSElement<T1, T2> {
    fn default() -> Self {
        TTSElement::new()
    }
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      T: Ord + Eq + Clone +Hash + Debug,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output=W> + Div<Output=W> + Zero + One + Debug>
    ApproximationStrategy<TreeStackInstruction<A>, PushDownInstruction<A>, T, W>
      for TTSElement<Transition<TreeStackInstruction<A>, T, W>, Transition<PushDownInstruction<A>, T, W>>{

    fn approximate_initial(&self, a: TreeStack<A>)-> PushDown<A>{
        let nempty = a.current_symbol().clone();
        let ele = vec![nempty.clone()];
        PushDown::from_vec(ele.clone())
    }

    fn approximate_transition(&mut self, t :  Transition<TreeStackInstruction<A>, T, W>) ->
        Transition<PushDownInstruction<A>, T, W>{
        match t.instruction{
            TreeStackInstruction::Up { ref current_val, ref new_val, ..}
            | TreeStackInstruction::Push { ref current_val, ref new_val, .. } => {
                let t2 = Transition {
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![current_val.clone()],
                        new_val: vec![current_val.clone(), new_val.clone()],
                    }
                };
                if !self.trans_map.contains_key(&t2) {
                    self.trans_map.insert(t2.clone(), Vec::new());
                    ()
                }
                self.trans_map.get_mut(&t2).unwrap().push(t.clone());
                t2
            }
            TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                let t2 = Transition {
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![current_val.clone(), old_val.clone()],
                        new_val: vec![new_val.clone()],
                    }
                };
                if !self.trans_map.contains_key(&t2) {
                    self.trans_map.insert(t2.clone(), Vec::new());
                    ()
                }
                self.trans_map.get_mut(&t2).unwrap().push(t.clone());
                t2
            }
        }
    }

    fn translate_run(&self, run: Vec<Transition<PushDownInstruction<A>, T, W>>) -> BinaryHeap<TreeStackTransitionSequence<A, T, W>> {
        let mut res = Vec::new();
        for lv in run{
            match self.trans_map.get(&lv){
                Some(v) =>{
                    if res.is_empty() {
                        res.push(v.clone())
                    }else{
                        let mut res2 = Vec::new();
                        for t in v{
                            for r1 in res.clone(){
                                let mut r2 = r1.clone();
                                r2.push(t.clone());
                                res2.push(r2);
                            }
                        }
                        res = res2;
                    }
                },
                None =>{
                    return BinaryHeap::new();
                },
            }
        }
        BinaryHeap::from(res)
    }

    fn add_transitions(&mut self, t1: &Transition<TreeStackInstruction<A>, T, W>, t2: &Transition<PushDownInstruction<A>, T, W>){
        if !self.trans_map.contains_key(&t2) {
            self.trans_map.insert(t2.clone(), Vec::new());
            ()
        }
        self.trans_map.get_mut(&t2).unwrap().push(t1.clone());
    }
}
