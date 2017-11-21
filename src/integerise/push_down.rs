use std::fmt::{self,Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::{Add, Mul, Div};
use std::vec::Vec;

use num_traits::{One, Zero};

use integeriser::{Integeriser, HashIntegeriser};

use automata::VecItem;
use cfg::*;
use approximation::*;
use integerise::*;

use coarse_to_fine;

use util::push_down::Pushdown;

// items of the transition system
type IntPushDownConfiguration<W> = Configuration<PushDown<usize>, usize, W>;
type IntPushDownTransition<W> = Transition<PushDown<usize>, PushDownInstruction<usize>, usize, W>;
type IntPushDownItem<W> = (IntPushDownConfiguration<W>, Pushdown<IntPushDownTransition<W>>);
type IntPushDownVecItem<W> = (IntPushDownConfiguration<W>, Vec<IntPushDownTransition<W>>);

// remove this when possible
type IntPushDownItemComplicated<'a, A, T, W> = IntItem<'a, PushDown<usize>, PushDownInstruction<usize>, A, T, W>;

// kinds of recognisers
type ExactIntPushDownRecogniser<'a, A, T, W> = IntRecogniser<'a, BinaryHeap<IntPushDownItem<W>>, PushDown<usize>, PushDownInstruction<usize>, A, T, W>;
type BeamIntPushDownRecogniser<'a, A, T, W> = IntRecogniser<'a, BoundedPriorityQueue<W, IntPushDownItem<W>>, PushDown<usize>, PushDownInstruction<usize>, A, T, W>;

/// Integerised Version of `PushDownAutomaton`. Holds two `Integeriser` used to encode the input, and the resulting `PushDownAutomaton`
#[derive(Clone)]
pub struct IntPushDownAutomaton<A: Ord + PartialEq + Clone + Hash, T: Eq + Hash, W: Ord + Eq>{
    pub term_integeriser: HashIntegeriser<T>,
    pub nterm_integeriser: HashIntegeriser<A>,
    pub automaton: PushDownAutomaton<usize,usize,W>,
}

impl<A: Ord + PartialEq + Clone + Hash,
     T: Eq + Clone + Hash,
     W: Ord + Eq + Clone + Add<Output=W> + Mul<Output=W> + Div<Output=W> + Zero + One>
    IntPushDownAutomaton<A, T, W>
{
    pub fn new(automaton: PushDownAutomaton<usize, usize, W>, inter1: HashIntegeriser<A>, inter2: HashIntegeriser<T>) -> IntPushDownAutomaton<A, T, W>
    {
        IntPushDownAutomaton{
            term_integeriser: inter2,
            automaton: automaton,
            nterm_integeriser: inter1,
        }
    }
}

impl<N: Clone + Debug + Hash + Ord + PartialEq,
     T: Clone + Debug + Hash + Ord + PartialEq,
     W: Clone + Debug + Ord + PartialEq + One + FromStr + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero>
    From<CFG<N, T, W>> for IntPushDownAutomaton<PushState<N, T>, T, W>
    where <W as FromStr>::Err: Debug{
     fn from(g: CFG<N, T, W>) -> Self {
         let mut inter1 = HashIntegeriser::new();
         let mut inter2 = HashIntegeriser::new();
         let a = PushDownAutomaton::from(g);
         let mut transitions= Vec::new();
         for t in a.list_transitions(){
             transitions.push(t.integerise(&mut inter1, &mut inter2));
         }
         IntPushDownAutomaton::new(
             PushDownAutomaton::new(transitions,
                                    a.initial.clone().integerise(&mut inter1)),
             inter1,
             inter2
         )
     }
 }


impl<A: Clone + Debug + Eq + Hash + Ord,
     T: Clone + Debug + Eq + Hash,
     W: Clone + Copy + Debug + Eq + Mul<Output=W> + One + Ord>
    IntegerisedAutomaton<PushDown<usize>, PushDownInstruction<usize>, A, T, W> for IntPushDownAutomaton<A, T, W> {
         type Key = A;

        fn recognise(&self, word: Vec<T>) -> ExactIntPushDownRecogniser<A, T, W> {
             let new_word = self.int_word(word);

             IntRecogniser{
                 term_integeriser: &self.term_integeriser,
                 nterm_integeriser: &self.nterm_integeriser,
                 recog: self.automaton.recognise(new_word)
             }
         }

        fn recognise_beam_search(&self, beam_width: usize, word: Vec<T>) -> BeamIntPushDownRecogniser<A, T, W> {
            let new_word = self.int_word(word);

            IntRecogniser{
                term_integeriser: &self.term_integeriser,
                nterm_integeriser: &self.nterm_integeriser,
                recog: self.automaton.recognise_beam_search(beam_width, new_word)
            }
        }

         fn check_run<'a>(&'a self, run: &[Transition<PushDown<usize>, PushDownInstruction<usize>, usize, W>]) -> Option<IntPushDownItemComplicated<A, T, W>> {
             let heap = self.automaton.check(self.automaton.initial().clone(), run);
             if heap.is_empty(){
                 return None;
             }
             let c = Configuration {
                 word: coarse_to_fine::run_word(run),
                 storage: heap[0].clone(),
                 weight: coarse_to_fine::run_weight(run),
             };
             Some(IntItem {
                 configuration: c,
                 run: Pushdown::from(run),
                 term_integeriser: &self.term_integeriser,
                 nterm_integeriser: &self.nterm_integeriser,
             })
         }

         fn int_word(&self, word: Vec<T>) -> Vec<usize>{
             let mut new_word = Vec::new();
             for e in word {
                 if let Some(x) = self.term_integeriser.find_key(&e) {
                     new_word.push(x)
                 }
             }
             new_word
         }
}

impl<A: Ord + Eq + Clone + Hash> Integerisable<PushDown<usize>, A> for PushDown<A> {
    fn integerise(&self, inter: &mut HashIntegeriser<A>) -> PushDown<usize> {
        let mut new_elements = Vec::new();
        for e in self.elements.clone(){
            new_elements.push(inter.integerise(e))
        }
        PushDown{
            elements: new_elements,
            empty: inter.integerise(self.empty.clone()),
        }
    }

    fn translate(s: PushDown<usize>, inter: &HashIntegeriser<A>)-> PushDown<A> {
        let mut new_elements = Vec::new();
        for e in s.elements.clone(){
            if let Some(x) = inter.find_value(e) {
                new_elements.push(x.clone())
            }
        }
        PushDown{
            elements: new_elements,
            empty: inter.find_value(s.empty).unwrap().clone(),
        }
    }
}

impl<A: Ord + PartialEq + Clone + Hash> Integerisable<PushDownInstruction<usize>, A> for PushDownInstruction<A>{
    fn integerise(&self, inter: &mut HashIntegeriser<A>) -> PushDownInstruction<usize> {
        match *self{
            PushDownInstruction::Replace {ref current_val, ref new_val} => {
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
            PushDownInstruction::ReplaceK {ref current_val, ref new_val, limit} => {
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
                    limit: limit,
                }
            }
        }
    }

    fn translate(s: PushDownInstruction<usize>, inter: &HashIntegeriser<A>)->PushDownInstruction<A>{
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
            PushDownInstruction::ReplaceK {ref current_val, ref new_val, limit} => {
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
                    limit: limit,
                }
            }
        }
    }
}

impl<A: Clone + Debug + Hash + Ord + PartialEq,
     B: Clone + Eq + Hash,
     W: Clone + Eq + Ord>
    IntegerisableM<Transition<PushDown<usize>, PushDownInstruction<usize>, usize, W>, A, B>
    for Transition<PushDown<A>, PushDownInstruction<A>, B, W>{
    fn integerise(&self, inter1: &mut HashIntegeriser<A>, inter2: &mut HashIntegeriser<B>)->Transition<PushDown<usize>, PushDownInstruction<usize>, usize, W>{
        let mut nword = Vec::new();
        for l in self.word.clone(){
            nword.push(inter2.integerise(l));
        }
        Transition {
            _dummy: PhantomData,
            word: nword,
            weight: self.weight.clone(),
            instruction: self.instruction.integerise(inter1),
        }
    }

    fn translate(s: Transition<PushDown<usize>, PushDownInstruction<usize>, usize, W>, inter1: &HashIntegeriser<A>, inter2: &HashIntegeriser<B>)->Transition<PushDown<A>, PushDownInstruction<A>, B, W>{
        let mut nword = Vec::new();
        for l in s.word.clone(){
            nword.push(inter2.find_value(l).unwrap().clone());
        }
        Transition {
            _dummy: PhantomData,
            word: nword,
            weight: s.weight.clone(),
            instruction: Integerisable::translate(s.instruction, inter1),
        }
    }
}

impl<A:  Ord + PartialEq + Clone + Hash, B: Eq + Hash + Clone,  W: Ord + Eq + Clone> IntegerisableM<Configuration<PushDown<usize>, usize, W>, A, B>
    for Configuration<PushDown<A>, B, W>{
    fn integerise(&self, inter1: &mut HashIntegeriser<A>, inter2: &mut HashIntegeriser<B>)->Configuration<PushDown<usize>, usize, W>{
        let mut nword = Vec::new();
        for l in self.word.clone(){
            nword.push(inter2.integerise(l));
        }
        Configuration {
            word: nword,
            storage: self.storage.integerise(inter1),
            weight: self.weight.clone(),
        }
    }

    fn translate(s: Configuration<PushDown<usize>, usize, W>, inter1: &HashIntegeriser<A>, inter2: &HashIntegeriser<B>)->Configuration<PushDown<A>, B, W>{
        let mut nword = Vec::new();
        for l in s.word.clone(){
            nword.push(inter2.find_value(l).unwrap().clone());
        }
        Configuration {
            word: nword,
            storage: Integerisable::translate(s.storage, inter1),
            weight: s.weight.clone(),
        }
    }
}

impl<A: Clone + Debug + Hash + Ord + PartialEq,
     T: Clone + Eq + Hash,
     W: Ord + Eq + Clone + Add<Output=W> + Mul<Output=W> + Div<Output=W> + Zero + One>
    IntegerisableM<PushDownAutomaton<usize, usize, W>, A, T>
    for PushDownAutomaton<A, T, W>{
    fn integerise(&self, inter1: &mut HashIntegeriser<A>, inter2: &mut HashIntegeriser<T>)->PushDownAutomaton<usize, usize, W>{
        let mut new_transitions = Vec::new();
        for l in self.list_transitions(){
            new_transitions.push(l.clone().integerise(inter1, inter2));
        }
        PushDownAutomaton::new(new_transitions, self.initial.integerise(inter1))
    }

    fn translate(s: PushDownAutomaton<usize, usize, W>, inter1: &HashIntegeriser<A>, inter2: &HashIntegeriser<T>)->PushDownAutomaton<A, T, W>{
        let mut new_transitions = Vec::new();
        for l in s.list_transitions(){
            new_transitions.push(IntegerisableM::translate(l.clone(), inter1, inter2));
        }
        PushDownAutomaton::new(new_transitions, Integerisable::translate(s.initial, inter1))
    }
}

impl <A: Clone + Debug + Hash + Ord + PartialEq,
      B: Clone + Debug + Hash + Ord + PartialEq,
      T: Eq + Clone + Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One,
      S: ApproximationStrategy<PushDown<A>, PushDown<B>,
                               Transition<PushDown<A>, PushDownInstruction<A>, usize, W>,
                               Transition<PushDown<B>, PushDownInstruction<B>, usize, W>>
      + IntApproximationStrategy<A, B, S2>,
      S2: ApproximationStrategy<PushDown<usize>, PushDown<usize>,
                                Transition<PushDown<usize>, PushDownInstruction<usize>, usize, W>,
                                Transition<PushDown<usize>, PushDownInstruction<usize>, usize, W>>>
      IntApproximation<S, S2, IntPushDownAutomaton<B, T, W>> for IntPushDownAutomaton<A, T, W>
      where W : Add<Output = W>{

    fn approximation(&self, strati: &S) -> Result<(IntPushDownAutomaton<B, T, W>, S2), String>{
        let (n_int, mut strat) = strati.integerise(&self.nterm_integeriser);
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

impl<A: Clone + Debug + Display + Hash + Ord + PartialEq,
     T: Clone + Debug + Eq + Hash,
     W: Clone + Copy + Debug + Display + Eq + One + Ord + Add<Output=W> + Mul<Output=W> + Div<Output=W> + Zero>
    Display for IntPushDownAutomaton<A, T, W> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            let t : PushDownAutomaton<A, T, W> = IntegerisableM::translate(self.automaton.clone(), &self.nterm_integeriser, &self.term_integeriser);
            write!(f, "{}", t)
        }
}

impl<'a,
     A: Clone + Debug + Hash + Ord + PartialEq,
     T: Clone + Eq + Hash,
     W: Clone + Eq + Ord>
    IntItem<'a, PushDown<usize>, PushDownInstruction<usize>, A, T, W> {
    pub fn translate(&self)-> VecItem<PushDown<A>, PushDownInstruction<A>, T, W> {
        let mut nvec = Vec::new();
        let vec: Vec<_> = self.run.clone().into();
        for t in vec {
            nvec.push(IntegerisableM::translate(t, self.nterm_integeriser, self.term_integeriser));
        }
        (IntegerisableM::translate(self.configuration.clone(), self.nterm_integeriser, self.term_integeriser), nvec)
    }

    pub fn give_up(&self)-> IntPushDownVecItem<W> {
        (self.configuration.clone(), self.run.clone().into())
    }
}
