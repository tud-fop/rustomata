use std::marker::PhantomData;
use std::collections::{BinaryHeap, BTreeMap};
use std::hash::Hash;

pub use automata::*;
pub use approximation::*;

pub use util::*;

pub use tree_stack::*;
pub use push_down::*;

//Strategy Element for mapping pushdown to its top most k elements
#[derive(Clone, Debug)]
pub struct TTSElement<A, T1, T2>{
    pub trans_map: BTreeMap<T2,Vec<T1>>,
    pub dummy: PhantomData<A>,
}

impl<A, T1, T2 : Ord> TTSElement<A, T1, T2>{
    pub fn new() -> TTSElement<A, T1, T2>{
        TTSElement{
            dummy : PhantomData,
            trans_map : BTreeMap::new(),
        }
    }
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      T: Ord + Eq + Clone +Hash + Debug,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<f64, Output=W> + Add<f64, Output = f64> + Zero + One + Debug> ApproximationStrategy<TreeStack<A>,PushDown<A>,
        automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>,
        automata::Transition<PushDown<A>,  PushDownInstruction<A>, T, W>>
      for TTSElement<A, automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>, TransitionKey<PushDown<A>,  PushDownInstruction<A>, T, W>>{

    fn approximate_initial(&self, a : TreeStack<A>)-> PushDown<A>{
        let np = Vec::new();
        let nempty = a.tree.get(&np).unwrap();
        let ele = vec![nempty.clone()];

        let p = PushDown{
            elements: ele.clone(),
            empty: ele[0].clone(),
        };
        p
    }

    fn approximate_transition(&mut self, t :  automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>) ->
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>{
        match t.instruction{
            TreeStackInstruction::Up { ref current_val, ref new_val, ..} => {
                let t2 = automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![current_val.clone()],
                        new_val: vec![current_val.clone(), new_val.clone()],
                    }
                };
                let tk = TransitionKey::new(&t2);
                if !self.trans_map.contains_key(&tk) {
                    self.trans_map.insert(tk.clone(), Vec::new());
                    ()
                }
                self.trans_map.get_mut(&tk).unwrap().push(t.clone());
                t2
            }
            TreeStackInstruction::Push { ref current_val, ref new_val, .. } => {
                let t2 = automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![current_val.clone()],
                        new_val: vec![current_val.clone(), new_val.clone()],
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
            TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                let t2 = automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::Replace {
                        current_val: vec![current_val.clone(), old_val.clone()],
                        new_val: vec![new_val.clone()],
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

    fn translate_run(&self, run: Vec<automata::Transition<PushDown<A>,  PushDownInstruction<A>, T, W>>) -> BinaryHeap<Vec<automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>>{
        let mut res = Vec::new();
        for lv in run{
            let lvk = TransitionKey::new(&lv);
            match self.trans_map.get(&lvk){
                Some(v) =>{
                    if res.len() == 0{
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

    fn add_transitions(&mut self, t1: &automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>, t2: &automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>){
        let tk = TransitionKey::new(t2);
        if !self.trans_map.contains_key(&tk) {
            self.trans_map.insert(tk.clone(), Vec::new());
            ()
        }
        self.trans_map.get_mut(&tk).unwrap().push(t1.clone());
    }
}

impl<N1: Ord + PartialEq + Debug + Clone + Hash, T: Ord, W: Ord>
    IntApproximationStrategy<N1, N1, TTSElement<u64, automata::Transition<TreeStack<u64>, TreeStackInstruction<u64>, T, W>,
                                                        TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>>
    for TTSElement<N1, automata::Transition<TreeStack<N1>, TreeStackInstruction<N1>, T, W>,
                          TransitionKey<PushDown<N1>, PushDownInstruction<N1>, T, W>>{

    fn integerise(&self, inter: &Integeriser<N1>)-> (Integeriser<N1>, TTSElement<u64, automata::Transition<TreeStack<u64>, TreeStackInstruction<u64>, T, W>,
                                         TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>){

         (inter.clone(), TTSElement::new())
    }
}
