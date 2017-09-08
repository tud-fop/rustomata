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
pub struct IntPushDownAutomaton<A: Ord + PartialEq + Debug + Clone + Hash, T: Eq + Hash, W: Ord + Eq>{
    pub term_integeriser: Integeriser<T>,
    pub nterm_integeriser: Integeriser<A>,
    pub automaton: PushDownAutomaton<u64,u64,W>,
}

impl<A: Ord + PartialEq + Debug + Clone + Hash,
    T: Eq + Clone + Hash,
    W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero +One> IntPushDownAutomaton<A, T, W>{
    pub fn new(automaton: PushDownAutomaton<u64, u64, W>, inter1: Integeriser<A>, inter2: Integeriser<T>)->IntPushDownAutomaton<A, T, W> {

        IntPushDownAutomaton{
            term_integeriser: inter2,
            automaton: automaton,
            nterm_integeriser: inter1,

        }
    }
}

impl<N: Clone + Debug + Ord + PartialEq + Hash,
     T: Clone + Debug + Ord + PartialEq + Hash,
     W: Clone + Debug + Ord + PartialEq + One + FromStr + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero
     > From<cfg::CFG<N, T, W>> for IntPushDownAutomaton<PushState<N, T>, T, W>
    where <W as FromStr>::Err: Debug{
     fn from(g: cfg::CFG<N, T, W>) -> Self {
         let mut inter1 = Integeriser::new();
         let mut inter2 = Integeriser::new();
         let a = PushDownAutomaton::from(g);
         let mut transitions= Vec::new();
         for t in a.list_transitions(){
             transitions.push(t.integerise(&mut inter1, &mut inter2));
         }
         IntPushDownAutomaton::new(PushDownAutomaton::new(transitions, a.initial.clone().integerise(&mut inter1)),inter1 ,inter2 )
     }
 }


impl<A: Ord + Eq + Debug + Clone + Hash,
      T: Clone + Debug + Eq + Hash,
      W: One + Mul<Output=W> + Clone + Copy + Debug + Eq + Ord>
     IntegerisedAutomaton<PushDown<u64>, PushDownInstruction<u64>, T, A, W> for IntPushDownAutomaton<A, T, W> {
         type Key = A;

         fn recognise<'a>(&'a self, word: Vec<T>) -> IntRecogniser<'a, PushDown<u64>, PushDownInstruction<u64>, T, A, W>{
             let new_word = self.int_word(word);

             IntRecogniser{
                 term_integeriser: &self.term_integeriser,
                 nterm_integeriser: &self.nterm_integeriser,
                 recog: self.automaton.recognise(new_word)
             }
         }

         fn check_run<'a>(&'a self, run: &Vec<Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>>, word: Vec<T>) -> Option<IntItem<'a, PushDown<u64>, PushDownInstruction<u64>, T, A, W>>{
             let new_word = self.int_word(word);
             let c = Configuration {
                 word: new_word,
                 storage: self.automaton.initial().clone(),
                 weight: W::one(),
             };

             match self.automaton.check(c, run){
                 Some(c2) => {
                     Some(IntItem{
                         configuration: c2,
                         run: run.clone(),
                         term_integeriser: &self.term_integeriser,
                         nterm_integeriser: &self.nterm_integeriser,
                     })
                 },
                 None => None,
             }
         }

         fn int_word(&self, word: Vec<T>)->Vec<u64>{
             let mut new_word = Vec::new();
             for e in word{
                 match self.term_integeriser.find_key(e){
                     Some(x) => new_word.push(x.clone()),
                     None => (),
                 }
             }
             return new_word;
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

    fn translate(s: PushDown<u64>, inter: &Integeriser<A>)-> PushDown<A>{
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

    fn translate(s: PushDownInstruction<u64>, inter: &Integeriser<A>)->PushDownInstruction<A>{
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

    fn translate(s: Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>, inter1: &Integeriser<A>, inter2: &Integeriser<B>)->Transition<PushDown<A>, PushDownInstruction<A>, B, W>{
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

    fn translate(s: Configuration<PushDown<u64>, u64, W>, inter1: &Integeriser<A>, inter2: &Integeriser<B>)->Configuration<PushDown<A>, B, W>{
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

impl<A:  Ord + PartialEq + Debug + Clone + Hash, T: Eq + Hash + Clone, W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One> IntegerisableM<PushDownAutomaton<u64, u64, W>, A, T>
    for PushDownAutomaton<A, T, W>{
    fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<T>)->PushDownAutomaton<u64, u64, W>{
        let mut new_transitions = Vec::new();
        for l in self.list_transitions(){
            new_transitions.push(l.clone().integerise(inter1, inter2));
        }
        PushDownAutomaton::new(new_transitions, self.initial.integerise(inter1))
    }

    fn translate(s: PushDownAutomaton<u64, u64, W>, inter1: &Integeriser<A>, inter2: &Integeriser<T>)->PushDownAutomaton<A, T, W>{
        let mut new_transitions = Vec::new();
        for l in s.list_transitions(){
            new_transitions.push(IntegerisableM::translate(l.clone(), inter1, inter2));
        }
        PushDownAutomaton::new(new_transitions, Integerisable::translate(s.initial, inter1))
    }
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One,
      S: ApproximationStrategy<PushDown<A>, PushDown<B>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, u64, W>,
        automata::Transition<PushDown<B>, PushDownInstruction<B>, u64, W>> +
        IntApproximationStrategy<A, B, S2>,
      S2: ApproximationStrategy<PushDown<u64>, PushDown<u64>,
        automata::Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>,
        automata::Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>>>
      IntApproximation<S, S2, IntPushDownAutomaton<B, T, W>> for IntPushDownAutomaton<A, T, W>
      where W : Add<Output = W>{

    fn approximation(&self, strati : &S) -> Result<(IntPushDownAutomaton<B, T, W>, S2), String>{
        let (n_int, mut strat) = strati.clone().integerise(&self.nterm_integeriser);
        let initial = strat.approximate_initial(self.automaton.initial.clone());
        let mut transitions = Vec::new();

        for (k, value) in self.automaton.transitions.clone(){
            if !(k == self.automaton.initial.empty){
                for t in &value{
                    let b = strat.approximate_transition(t.clone());
                    transitions.push(b);
                }
            }
        }
        let a = PushDownAutomaton::new(
            transitions,
            initial,
        );
        let b = IntPushDownAutomaton::new(a, n_int, self.term_integeriser.clone());
        Ok((b, strat))
    }
}

impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash + fmt::Display,
     T: Clone + fmt::Debug + Eq + Hash,
     W: One + Clone + Copy + fmt::Debug + Eq + Ord + fmt::Display + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero>
    fmt::Display for IntPushDownAutomaton<A, T, W> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let t : PushDownAutomaton<A, T, W> = IntegerisableM::translate(self.automaton.clone(), &self.nterm_integeriser, &self.term_integeriser);
            write!(f, "{}", t)
        }
}

impl<'a,
   A: Ord + PartialEq + Debug + Clone + Hash,
   T: Eq + Clone + Hash + Debug,
   W: Ord + Eq + Clone + Debug> IntItem<'a, PushDown<u64>, PushDownInstruction<u64>, T, A, W>{
    pub fn translate(&self)->(Configuration<PushDown<A>, T, W>, Vec<Transition<PushDown<A>, PushDownInstruction<A>, T, W>>){
        let mut nvec = Vec::new();
        for t in self.run.clone(){
            nvec.push(IntegerisableM::translate(t, self.nterm_integeriser, self.term_integeriser));
        }
        (IntegerisableM::translate(self.configuration.clone(), self.nterm_integeriser, self.term_integeriser), nvec)
    }

    pub fn give_up(&self)->(Configuration<PushDown<u64>, u64, W>, Vec<Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>>){
        (self.configuration.clone(), self.run.clone())
    }
}
