use std::marker::PhantomData;
use std::collections::{BinaryHeap, BTreeMap};
use std::hash::Hash;

pub use automata::*;
pub use approximation::*;

pub use util::*;
pub use util::integeriser::*;

pub use tree_stack::*;
pub use push_down::*;

//Strategy Element for mapping pushdown to its top most k elements
#[derive(Clone, Debug)]
pub struct PDTopKElement<A, T1, T2>{
    pub dummy: PhantomData<A>,
    pub trans_map: BTreeMap<T2,Vec<T1>>,
    pub size: usize,
}

impl<A, T1, T2 : Ord> PDTopKElement<A, T1, T2>{
    pub fn new(size: usize) -> PDTopKElement<A, T1, T2>{
        PDTopKElement{
            dummy : PhantomData,
            trans_map : BTreeMap::new(),
            size : size,
        }
    }
}

impl <A : Ord + PartialEq + Debug + Clone + Hash,
      T: Ord + Eq + Clone + Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One> ApproximationStrategy<PushDown<A>, PushDown<A>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>
      for PDTopKElement<A, automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>, TransitionKey<PushDown<A>, PushDownInstruction<A>, T, W>>{

    fn approximate_initial(&self, a : PushDown<A>)-> PushDown<A>{
        let mut b = a.elements.clone();
        b.remove(0);
        let pushdown = PushDown::new(b[0].clone(), a.empty.clone());
        let ps=pushdown.replacek(&b, &b, &self.size.clone());
        ps[0].clone()
    }

    fn approximate_transition(&mut self, t :  automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>) ->
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>{
        match t.instruction{
            PushDownInstruction::Replace {ref current_val, ref new_val} => {
                let t2 = automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::ReplaceK {
                        current_val: current_val.clone(),
                        new_val: new_val.clone(),
                        limit: self.size.clone(),
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
            PushDownInstruction::ReplaceK {ref current_val, ref new_val, ..} => {
                let t2 = automata::Transition {
                    _dummy: PhantomData,
                    word: t.word.clone(),
                    weight: t.weight.clone(),
                    instruction: PushDownInstruction::ReplaceK {
                        current_val: current_val.clone(),
                        new_val: new_val.clone(),
                        limit: self.size.clone(),
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

    fn translate_run(&self, run: Vec<automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>) -> BinaryHeap<Vec<automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>>>{
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

    fn add_transitions(&mut self, t1: &automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>, t2: &automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>){
        let tk = TransitionKey::new(t2);
        if !self.trans_map.contains_key(&tk) {
            self.trans_map.insert(tk.clone(), Vec::new());
            ()
        }
        self.trans_map.get_mut(&tk).unwrap().push(t1.clone());
    }
}

impl<N1: Ord + PartialEq + Debug + Clone + Hash, T: Ord, W: Ord> IntApproximationStrategy<N1, N1, PDTopKElement<u64, automata::Transition<PushDown<u64>, PushDownInstruction<u64>, T, W>,
                                                                       TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>>
    for PDTopKElement<N1, automata::Transition<PushDown<N1>, PushDownInstruction<N1>, T, W>,
                          TransitionKey<PushDown<N1>, PushDownInstruction<N1>, T, W>>{

    fn integerise(&self, inter: &Integeriser<N1>)-> (Integeriser<N1>, PDTopKElement<u64, automata::Transition<PushDown<u64>, PushDownInstruction<u64>, T, W>,
                                         TransitionKey<PushDown<u64>, PushDownInstruction<u64>, T, W>>){

         (inter.clone(), PDTopKElement::new(self.size))
    }
}
