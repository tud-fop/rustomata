use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use num_traits::{One, Zero};
use std::ops::{Add, Mul};
use std::collections::HashMap;

use automata;
use pmcfg;

pub use integerise::*;

#[derive(Clone)]
pub struct IntTreeStackAutomaton<A: Ord + PartialEq + Debug + Clone + Hash, T: Eq, W: Ord + Eq>{
    pub term_integeriser: Integeriser<T>,
    pub nterm_integeriser: Integeriser<A>,
    pub automaton: TreeStackAutomaton<u64,u64,W>,
    pub old_automaton: TreeStackAutomaton<A, T, W>,
}

impl<A: Ord + PartialEq + Debug + Clone + Hash,
    T: Eq + Clone + Hash,
    W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Zero +One> IntTreeStackAutomaton<A, T, W>{
    pub fn new(transitions: Vec<automata::Transition<TreeStack<A>,TreeStackInstruction<A>, T, W>>, initial: TreeStack<A>, mut inter1: Integeriser<A>, mut inter2: Integeriser<T>)->IntTreeStackAutomaton<A, T, W> {
        let mut new_transitions = Vec::new();
        for t in transitions.clone(){
            new_transitions.push(t.integerise(&mut inter1, &mut inter2));
        }
        IntTreeStackAutomaton{
            term_integeriser: inter2,
            automaton: TreeStackAutomaton::new(new_transitions, initial.clone().integerise(&mut inter1)),
            old_automaton: TreeStackAutomaton::new(transitions, initial),
            nterm_integeriser: inter1,

        }
    }
}

impl<N: Clone + Debug + Ord + PartialEq + Hash,
     T: Clone + Debug + Ord + PartialEq + Hash,
     W: Clone + Debug + Ord + PartialEq + One + FromStr + Add<Output=W> + Mul<Output = W> + Zero
     > From<pmcfg::PMCFG<N, T, W>> for IntTreeStackAutomaton<PosState<pmcfg::PMCFGRule<N, T, W>>, T, W>
    where <W as FromStr>::Err: Debug{
     fn from(g: pmcfg::PMCFG<N, T, W>) -> Self {
         let a = TreeStackAutomaton::from(g);
         let mut transitions= Vec::new();
         for t in a.list_transitions(){
             transitions.push(t.clone());
         }
         IntTreeStackAutomaton::new(transitions, a.initial, Integeriser::new(), Integeriser::new())
     }
 }


 impl<A: Ord + Eq + Debug + Clone + Hash,
       T: Clone + Debug + Eq + Hash,
       W: One + Mul<Output=W> + Clone + Copy + Debug + Eq + Ord>
      IntegerisedAutomaton<TreeStack<u64>, TreeStackInstruction<u64>, T, A, W> for IntTreeStackAutomaton<A, T, W> {
          type Key = A;

          fn recognise<'a>(&'a self, word: Vec<T>) -> IntRecogniser<'a, TreeStack<u64>, TreeStackInstruction<u64>, T, A, W>{
              let mut new_word = Vec::new();
              for e in word{
                  match self.term_integeriser.find_key(e){
                      Some(x) => new_word.push(x.clone()),
                      None => (),
                  }
              }

              IntRecogniser{
                  term_integeriser: self.term_integeriser.clone(),
                  nterm_integeriser: self.nterm_integeriser.clone(),
                  recog: self.automaton.recognise(new_word)
              }
          }
 }

 impl<'a, T: Clone + Debug + Eq + Hash,
          A: Ord + Eq + Debug + Clone + Hash,
          W: Ord + Clone + Debug> Iterator for IntRecogniser<'a, TreeStack<u64>, TreeStackInstruction<u64>, T, A, W> {
     type Item = (Configuration<TreeStack<A>, T, W>, Vec<Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>);

     fn next(&mut self) -> Option<(Configuration<TreeStack<A>, T, W>, Vec<Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>)> {
         match self.recog.next(){
             Some((c, run)) =>{
                 let mut nrun = Vec::new();
                 for t in run{
                     nrun.push(IntegerisableM::translate(t, &mut self.nterm_integeriser, &mut self.term_integeriser))
                 }
                 Some((IntegerisableM::translate(c, &mut self.nterm_integeriser, &mut self.term_integeriser), nrun))
             },
             None => None
         }
     }
 }


 impl<A: Ord + PartialEq + Debug + Clone + Hash> Integerisable<TreeStack<u64>, A> for TreeStack<A>{
     fn integerise(&self, inter: &mut Integeriser<A>)->TreeStack<u64>{
         let mut new_tree = HashMap::new();
         for (key, value) in self.tree.clone(){
             new_tree.insert(key, inter.integerise(value));
         }
         TreeStack{
             tree: new_tree,
             pointer: self.pointer.clone()
         }
     }

     fn translate(s: TreeStack<u64>, inter: &mut Integeriser<A>)->TreeStack<A>{
         let mut new_tree = HashMap::new();
         for (key, value) in s.tree.clone(){
             new_tree.insert(key, inter.find_value(value).unwrap().clone());
         }
         TreeStack{
             tree: new_tree,
             pointer: s.pointer.clone()
         }
     }
 }

 impl<A: Ord + PartialEq + Debug + Clone + Hash> Integerisable<TreeStackInstruction<u64>, A> for TreeStackInstruction<A>{
     fn integerise(&self, inter: &mut Integeriser<A>)->TreeStackInstruction<u64>{
         match self{
             &TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } => {
                 TreeStackInstruction::Up {
                     n: n,
                     current_val: inter.integerise(current_val.clone()),
                     old_val: inter.integerise(old_val.clone()),
                     new_val: inter.integerise(new_val.clone()),
                 }
             }
             &TreeStackInstruction::Push { n, ref current_val, ref new_val } => {
                 TreeStackInstruction::Push {
                     n: n,
                     current_val: inter.integerise(current_val.clone()),
                     new_val: inter.integerise(new_val.clone()),
                 }
             }
             &TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                 TreeStackInstruction::Down {
                     current_val: inter.integerise(current_val.clone()),
                     old_val: inter.integerise(old_val.clone()),
                     new_val: inter.integerise(new_val.clone()),
                 }
             }
         }
     }

     fn translate(s: TreeStackInstruction<u64>, inter: &mut Integeriser<A>)->TreeStackInstruction<A>{
         match s{
             TreeStackInstruction::Up { n, ref current_val, ref old_val, ref new_val } => {
                 TreeStackInstruction::Up {
                     n: n,
                     current_val: inter.find_value(*current_val).unwrap().clone(),
                     old_val: inter.find_value(*old_val).unwrap().clone(),
                     new_val: inter.find_value(*new_val).unwrap().clone(),
                 }
             }
             TreeStackInstruction::Push { n, ref current_val, ref new_val } => {
                 TreeStackInstruction::Push {
                     n: n,
                     current_val: inter.find_value(*current_val).unwrap().clone(),
                     new_val: inter.find_value(*new_val).unwrap().clone(),
                 }
             }
             TreeStackInstruction::Down { ref current_val, ref old_val, ref new_val } => {
                 TreeStackInstruction::Down {
                     current_val: inter.find_value(*current_val).unwrap().clone(),
                     old_val: inter.find_value(*old_val).unwrap().clone(),
                     new_val: inter.find_value(*new_val).unwrap().clone(),
                 }
             }
         }
     }
 }

 impl<A:  Ord + PartialEq + Debug + Clone + Hash, B: Eq + Hash + Clone,  W: Ord + Eq + Clone> IntegerisableM<Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>, A, B>
     for Transition<TreeStack<A>, TreeStackInstruction<A>, B, W>{
     fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>{
         let mut nword = Vec::new();
         for l in self.word.clone(){
             nword.push(inter2.integerise(l));
         }
         automata::Transition {
             _dummy: PhantomData,
             word: nword,
             weight: self.weight.clone(),
             instruction: self.instruction.integerise(inter1),
         }
     }

     fn translate(s: Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Transition<TreeStack<A>, TreeStackInstruction<A>, B, W>{
         let mut nword = Vec::new();
         for l in s.word.clone(){
             nword.push(inter2.find_value(l).unwrap().clone());
         }
         automata::Transition {
             _dummy: PhantomData,
             word: nword,
             weight: s.weight.clone(),
             instruction: Integerisable::translate(s.instruction, inter1),
         }
     }
 }

impl<A:  Ord + PartialEq + Debug + Clone + Hash, B: Eq + Hash + Clone,  W: Ord + Eq + Clone> IntegerisableM<Configuration<TreeStack<u64>, u64, W>, A, B>
     for Configuration<TreeStack<A>, B, W>{
     fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Configuration<TreeStack<u64>, u64, W>{
         let mut nword = Vec::new();
         for l in self.word.clone(){
             nword.push(inter2.integerise(l));
         }
         automata::Configuration {
             word: nword,
             storage: self.storage.integerise(inter1),
             weight: self.weight.clone(),
         }
     }

     fn translate(s: Configuration<TreeStack<u64>, u64, W>, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Configuration<TreeStack<A>, B, W>{
         let mut nword = Vec::new();
         for l in s.word.clone(){
             nword.push(inter2.find_value(l).unwrap().clone());
         }
         automata::Configuration {
             word: nword,
             storage: Integerisable::translate(s.storage, inter1),
             weight: s.weight.clone(),
         }
     }
 }


 impl <A: Ord + PartialEq + Debug + Clone + Hash,
       B: Ord + PartialEq + Debug + Clone + Hash,
       T: Eq + Clone +Hash,
       W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Zero + One,
       S: Clone + ApproximationStrategy<TreeStack<A>, PushDown<B>,
         automata::Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>,
         automata::Transition<PushDown<B>, PushDownInstruction<B>, T, W>>>
       Approximation<S, IntPushDownAutomaton<B, T, W>> for IntTreeStackAutomaton<A, T, W>
       where W : Add<Output = W>{

     fn approximation(&self, strat : &S) -> Result<IntPushDownAutomaton<B, T, W>, String>{
         let new_automaton = self.old_automaton.approximation(strat);

         match new_automaton{
             Ok(a) =>{
                 let mut transitions= Vec::new();
                 for t in a.list_transitions(){
                     transitions.push(t.clone());
                 }
                 Ok(IntPushDownAutomaton::new(transitions, a.initial, Integeriser::new(), Integeriser::new()))
             }
             Err(e) => Err(e)
         }

     }
 }
