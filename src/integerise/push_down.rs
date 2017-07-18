use std::fmt::Debug;
use std::fmt;
use std::hash::Hash;
use std::vec::Vec;
use num_traits::{One, Zero};
use std::ops::{Add, Mul, Div};

use automata;
use cfg;

pub use integerise::*;

#[derive(Clone)]
pub struct IntPushDownAutomaton<A: Ord + PartialEq + Debug + Clone + Hash, T: Eq, W: Ord + Eq>{
    pub term_integeriser: Integeriser<T>,
    pub nterm_integeriser: Integeriser<A>,
    pub automaton: PushDownAutomaton<u64,u64,W>,
    pub old_automaton: PushDownAutomaton<A, T, W>,
}

impl<A: Ord + PartialEq + Debug + Clone + Hash,
    T: Eq + Clone + Hash,
    W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero +One> IntPushDownAutomaton<A, T, W>{
    pub fn new(transitions: Vec<automata::Transition<PushDown<A>,PushDownInstruction<A>, T, W>>, initial: PushDown<A>, mut inter1: Integeriser<A>, mut inter2: Integeriser<T>)->IntPushDownAutomaton<A, T, W> {
        let mut new_transitions = Vec::new();
        for t in transitions.clone(){
            new_transitions.push(t.integerise(&mut inter1, &mut inter2));
        }
        IntPushDownAutomaton{
            term_integeriser: inter2,
            automaton: PushDownAutomaton::new(new_transitions, initial.clone().integerise(&mut inter1)),
            old_automaton: PushDownAutomaton::new(transitions, initial),
            nterm_integeriser: inter1,

        }
    }
}

impl<N: Clone + Debug + Ord + PartialEq + Hash,
     T: Clone + Debug + Ord + PartialEq + Hash,
     W: Clone + Debug + Ord + PartialEq + One + FromStr + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero
     > From<cfg::CFG<N, T, W>> for IntPushDownAutomaton<PushState<N,T>, T, W>
    where <W as FromStr>::Err: Debug{
    fn from(g: cfg::CFG<N, T, W>) -> Self {
         let a = PushDownAutomaton::from(g);
         let mut transitions= Vec::new();
         for t in a.list_transitions(){
             transitions.push(t.clone());
         }
         IntPushDownAutomaton::new(transitions, a.initial, Integeriser::new(), Integeriser::new())
     }
}


impl<A: Ord + Eq + Debug + Clone + Hash,
      T: Clone + Debug + Eq + Hash,
      W: One + Mul<Output=W> + Clone + Copy + Debug + Eq + Ord>
     IntegerisedAutomaton<PushDown<u64>, PushDownInstruction<u64>, T, A, W> for IntPushDownAutomaton<A, T, W> {
         type Key = A;

         fn recognise<'a>(&'a self, word: Vec<T>) -> IntRecogniser<'a, PushDown<u64>, PushDownInstruction<u64>, T, A, W>{
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
         W: Ord + Clone + Debug> Iterator for IntRecogniser<'a, PushDown<u64>, PushDownInstruction<u64>, T, A, W> {
    type Item = (Configuration<PushDown<A>, T, W>, Vec<Transition<PushDown<A>, PushDownInstruction<A>, T, W>>);

    fn next(&mut self) -> Option<(Configuration<PushDown<A>, T, W>, Vec<Transition<PushDown<A>, PushDownInstruction<A>, T, W>>)> {
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

impl<A: Ord + Eq + Debug + Clone + Hash> Integerisable<PushDown<u64>, A> for PushDown<A>{
    fn integerise(&self, inter: &mut Integeriser<A>)->PushDown<u64>{
        let mut new_elements = Vec::new();
        for e in self.elements.clone(){
            new_elements.push(inter.integerise(e))
        }
        PushDown{
            elements: new_elements,
            empty: inter.integerise(self.empty.clone()),
        }
    }

    fn translate(s: PushDown<u64>, inter: &mut Integeriser<A>)-> PushDown<A>{
        let mut new_elements = Vec::new();
        for e in s.elements.clone(){
            match inter.find_value(e){
                Some(x) => new_elements.push(x.clone()),
                None => (),
            }
        }
        PushDown{
            elements: new_elements,
            empty: inter.find_value(s.empty).unwrap().clone(),
        }
    }
}

impl<A: Ord + PartialEq + Debug + Clone + Hash> Integerisable<PushDownInstruction<u64>, A> for PushDownInstruction<A>{
    fn integerise(&self, inter: &mut Integeriser<A>)->PushDownInstruction<u64>{
        match self{
            &PushDownInstruction::Replace {ref current_val, ref new_val} => {
                let mut ncv = Vec::new();
                let mut nnv = Vec::new();
                for e in current_val{
                    ncv.push(inter.integerise(e.clone()))
                }
                for e in new_val{
                    nnv.push(inter.integerise(e.clone()))
                }
                PushDownInstruction::Replace{
                    current_val:ncv,
                    new_val:nnv,
                }
            }
            &PushDownInstruction::ReplaceK {ref current_val, ref new_val, ref limit} => {
                let mut ncv = Vec::new();
                let mut nnv = Vec::new();
                for e in current_val{
                    ncv.push(inter.integerise(e.clone()))
                }
                for e in new_val{
                    nnv.push(inter.integerise(e.clone()))
                }
                PushDownInstruction::ReplaceK{
                    current_val:ncv,
                    new_val:nnv,
                    limit: limit.clone(),
                }
            }
        }
    }

    fn translate(s: PushDownInstruction<u64>, inter: &mut Integeriser<A>)->PushDownInstruction<A>{
        match s{
            PushDownInstruction::Replace {ref current_val, ref new_val} => {
                let mut ncv = Vec::new();
                let mut nnv = Vec::new();
                for e in current_val{
                    ncv.push(inter.find_value(*e).unwrap().clone())
                }
                for e in new_val{
                    nnv.push(inter.find_value(*e).unwrap().clone())
                }
                PushDownInstruction::Replace{
                    current_val:ncv,
                    new_val:nnv,
                }
            }
            PushDownInstruction::ReplaceK {ref current_val, ref new_val, ref limit} => {
                let mut ncv = Vec::new();
                let mut nnv = Vec::new();
                for e in current_val{
                    ncv.push(inter.find_value(*e).unwrap().clone())
                }
                for e in new_val{
                    nnv.push(inter.find_value(*e).unwrap().clone())
                }
                PushDownInstruction::ReplaceK{
                    current_val:ncv,
                    new_val:nnv,
                    limit: limit.clone(),
                }
            }
        }
    }
}

impl<A:  Ord + PartialEq + Debug + Clone + Hash, B: Eq + Hash + Clone,  W: Ord + Eq + Clone> IntegerisableM<Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>, A, B>
    for Transition<PushDown<A>, PushDownInstruction<A>, B, W>{
    fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>{
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

    fn translate(s: Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Transition<PushDown<A>, PushDownInstruction<A>, B, W>{
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

impl<A:  Ord + PartialEq + Debug + Clone + Hash, B: Eq + Hash + Clone,  W: Ord + Eq + Clone> IntegerisableM<Configuration<PushDown<u64>, u64, W>, A, B>
    for Configuration<PushDown<A>, B, W>{
    fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Configuration<PushDown<u64>, u64, W>{
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

    fn translate(s: Configuration<PushDown<u64>, u64, W>, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Configuration<PushDown<A>, B, W>{
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
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One,
      S: ApproximationStrategy<PushDown<A>, PushDown<B>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
        automata::Transition<PushDown<B>, PushDownInstruction<B>, T, W>>>
      Approximation<S, IntPushDownAutomaton<B, T, W>> for IntPushDownAutomaton<A, T, W>
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

impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash + fmt::Display,
     T: Clone + fmt::Debug + Eq + Hash,
     W: One + Clone + Copy + fmt::Debug + Eq + Ord + fmt::Display + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero>
    fmt::Display for IntPushDownAutomaton<A, T, W> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.old_automaton)
        }
}
