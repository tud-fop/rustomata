use std::fmt::Debug;
use std::fmt;
use std::hash::Hash;
use std::vec::Vec;
use num_traits::{One, Zero};
use std::ops::{Add, Mul, Div};

use automata::*;
use tree_stack::*;
use approximation::*;

use integerise::*;

/// Integerised version of the `TreeStackAutomaton`. Holds the two `Integeriser` used to encode the input and the resulting `TreeStackAutomaton`
#[derive(Clone)]
pub struct IntTreeStackAutomaton<A: Ord + PartialEq + Debug + Clone + Hash, T: Eq + Hash, W: Ord + Eq>{
    pub term_integeriser: Integeriser<T>,
    pub nterm_integeriser: Integeriser<A>,
    pub automaton: TreeStackAutomaton<u64,u64,W>,
}

impl<A: Ord + PartialEq + Debug + Clone + Hash,
     T: Eq + Clone + Hash + Debug,
     W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero +One> IntTreeStackAutomaton<A, T, W>{
    pub fn new(automaton: TreeStackAutomaton<u64, u64, W>, inter1: Integeriser<A>, inter2: Integeriser<T>)->IntTreeStackAutomaton<A, T, W> {
        IntTreeStackAutomaton{
            term_integeriser: inter2,
            automaton: automaton,
            nterm_integeriser: inter1,

        }
    }
}

impl<A: Ord + Eq + Debug + Clone + Hash,
     T: Clone + Debug + Eq + Hash,
     W: One + Mul<Output=W> + Clone + Copy + Debug + Eq + Ord>
    IntegerisedAutomaton<TreeStack<u64>, TreeStackInstruction<u64>, T, A, W> for IntTreeStackAutomaton<A, T, W> {
        type Key = A;

        fn recognise<'a>(&'a self, word: Vec<T>) -> IntRecogniser<'a, TreeStack<u64>, TreeStackInstruction<u64>, T, A, W>{
            let new_word = self.int_word(word);

            IntRecogniser{
                term_integeriser: &self.term_integeriser,
                nterm_integeriser: &self.nterm_integeriser,
                recog: self.automaton.recognise(new_word)
            }
        }

        fn check_run<'a>(&'a self, run: &Vec<Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>>) -> Option<IntItem<'a, TreeStack<u64>, TreeStackInstruction<u64>, T, A, W>>{
            let heap = self.automaton.check(self.automaton.initial().clone(), run);
            if heap.is_empty(){
                return None;
            }
            let c = Configuration {
                word: ctf::run_word(&run),
                storage: heap[0].clone(),
                weight: ctf::run_weight(&run),
            };
            Some(IntItem{
                configuration: c,
                run: Pushdown::from(run.clone()),
                term_integeriser: &self.term_integeriser,
                nterm_integeriser: &self.nterm_integeriser,
            })
        }

        fn int_word(&self, word: Vec<T>)-> Vec<u64>{
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

impl<A: Ord + PartialEq + Debug + Clone + Hash> Integerisable<TreeStack<u64>, A> for TreeStack<A>{
    fn integerise(&self, inter: &mut Integeriser<A>) -> TreeStack<u64>{
        self.map_mut(&mut move |v| inter.integerise(v.clone()))
    }

    fn translate(s: TreeStack<u64>, inter: &Integeriser<A>) -> TreeStack<A>{
        s.map(&|&v| inter.find_value(v).unwrap().clone())
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

    fn translate(s: TreeStackInstruction<u64>, inter: &Integeriser<A>)->TreeStackInstruction<A>{
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

impl<A:  Ord + PartialEq + Debug + Clone + Hash, B: Eq + Hash + Clone + Debug,  W: Ord + Eq + Clone> IntegerisableM<Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>, A, B>
    for Transition<TreeStack<A>, TreeStackInstruction<A>, B, W>{
        fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>{
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

        fn translate(s: Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>, inter1: &Integeriser<A>, inter2: &Integeriser<B>)->Transition<TreeStack<A>, TreeStackInstruction<A>, B, W>{
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

impl<A:  Ord + PartialEq + Debug + Clone + Hash, B: Eq + Hash + Clone + Debug,  W: Ord + Eq + Clone> IntegerisableM<Configuration<TreeStack<u64>, u64, W>, A, B>
    for Configuration<TreeStack<A>, B, W>{
        fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->Configuration<TreeStack<u64>, u64, W>{
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

        fn translate(s: Configuration<TreeStack<u64>, u64, W>, inter1: &Integeriser<A>, inter2: &Integeriser<B>)->Configuration<TreeStack<A>, B, W>{
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
     B: Clone + Debug + Eq + Hash,
     W: Ord + Eq + Clone> IntegerisableM<TreeStackAutomaton<u64, u64, W>, A, B>
    for TreeStackAutomaton<A, B, W>{
        fn integerise(&self, inter1: &mut Integeriser<A>, inter2: &mut Integeriser<B>)->TreeStackAutomaton<u64, u64, W>{
            let mut new_transitions = Vec::new();
            for l in self.list_transitions(){
                new_transitions.push(l.integerise(inter1, inter2));
            }
            TreeStackAutomaton::new(new_transitions, self.initial.integerise(inter1))
        }
        fn translate(s: TreeStackAutomaton<u64, u64, W>, inter1: &Integeriser<A>, inter2: &Integeriser<B>)->TreeStackAutomaton<A, B, W>{
            let mut new_transitions = Vec::new();
            for l in s.list_transitions(){
                new_transitions.push(IntegerisableM::translate(l.clone(), inter1, inter2));
            }
            TreeStackAutomaton::new(new_transitions, Integerisable::translate(s.initial, inter1))
        }
    }


impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone + Hash + Debug,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One,
      S: Clone + ApproximationStrategy<TreeStack<A>, PushDown<B>,
                                       Transition<TreeStack<A>, TreeStackInstruction<A>, u64, W>,
                                       Transition<PushDown<B>, PushDownInstruction<B>, u64, W>>
      + IntApproximationStrategy<A, B, S2>,
      S2: ApproximationStrategy<TreeStack<u64>, PushDown<u64>,
                                Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>,
                                Transition<PushDown<u64>, PushDownInstruction<u64>, u64, W>>>
    IntApproximation<S, S2, IntPushDownAutomaton<B, T, W>> for IntTreeStackAutomaton<A, T, W>
    where W : Add<Output = W>{
    
    fn approximation(&self, strati : &S) -> Result<(IntPushDownAutomaton<B, T, W>, S2), String>{
        let (n_int, mut strat) = strati.clone().integerise(&self.nterm_integeriser);
        let initial1 = strat.approximate_initial(self.automaton.initial.clone());
        let i = self.automaton.initial.current_symbol();
        let mut fina = initial1.empty.clone();
        let mut transitions = Vec::new();

        for (_, value) in self.automaton.transitions.clone(){
            for t in &value{
                match t.instruction{
                    TreeStackInstruction::Down { ref old_val, .. }=>{
                        let b = strat.approximate_transition(t.clone());

                        if *old_val == *i{
                            match b.instruction{
                                PushDownInstruction::Replace {ref current_val, ref new_val} =>{
                                    fina = new_val[0].clone();
                                    let st = Transition {
                                        _dummy: PhantomData,
                                        word: b.word.clone(),
                                        weight: b.weight.clone(),
                                        instruction: PushDownInstruction::Replace {
                                            current_val: current_val.clone(),
                                            new_val: Vec::new(),
                                        }
                                    };
                                    strat.add_transitions(t, &st);
                                    transitions.push(st);
                                },
                                _=>{
                                    transitions.push(b.clone());
                                },
                            }
                        }else{
                            transitions.push(b.clone());
                        }
                    },
                    _=> {
                        let b = strat.approximate_transition(t.clone());
                        transitions.push(b);
                    },
                }

            }
        }
        let initial2 = PushDown::new(initial1.empty, fina);

        let a = PushDownAutomaton::new(
            transitions,
            initial2
        );
        let b = IntPushDownAutomaton::new(a, n_int, self.term_integeriser.clone()); // TODO actual integeriser

        Ok((b,strat))
    }
}

impl<A: Ord + PartialEq + fmt::Debug + Clone + Hash + fmt::Display,
     T: Clone + fmt::Debug + Eq + Hash,
     W: One + Clone + Copy + fmt::Debug + Eq + Ord + fmt::Display + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero>
    fmt::Display for IntTreeStackAutomaton<A, T, W> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let t : TreeStackAutomaton<A, T, W> = IntegerisableM::translate(self.automaton.clone(), &self.nterm_integeriser, &self.term_integeriser);
            write!(f, "{}", t)
        }
    }

impl<'a,
     A: Ord + PartialEq + Debug + Clone + Hash,
     T: Eq + Clone + Hash + Debug,
     W: Ord + Eq + Clone + Debug> IntItem<'a, TreeStack<u64>, TreeStackInstruction<u64>, T, A, W>{
    pub fn translate(&self)->(Configuration<TreeStack<A>, T, W>, Vec<Transition<TreeStack<A>, TreeStackInstruction<A>, T, W>>){
        let mut nvec = Vec::new();
        let vec: Vec<_> = self.run.clone().into();
        for t in vec {
            nvec.push(IntegerisableM::translate(t, self.nterm_integeriser, self.term_integeriser));
        }
        (IntegerisableM::translate(self.configuration.clone(), self.nterm_integeriser, self.term_integeriser), nvec)
    }

    pub fn give_up(&self)->(Configuration<TreeStack<u64>, u64, W>, Vec<Transition<TreeStack<u64>, TreeStackInstruction<u64>, u64, W>>){
        (self.configuration.clone(), self.run.clone().into())
    }
}
