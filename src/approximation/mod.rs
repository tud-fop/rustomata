use std::marker::PhantomData;
use std::collections::{BinaryHeap};
use std::hash::Hash;
use std::fmt::Debug;
use std::ops::{Add, Mul, Div};
use num_traits::{Zero, One};

pub mod relabel;
pub mod ptk;
pub mod tts;

use automata;

use tree_stack::*;
use push_down::*;

pub use self::relabel::*;
pub use self::ptk::*;
pub use self::tts::*;

/// Object defining the strategies used for `approximation`
pub trait ApproximationStrategy<A1, A2, T1, T2> {
    fn approximate_initial(&self, A1) -> A2;

    fn add_transitions(&mut self, &T1, &T2);

    fn approximate_transition(&mut self, T1) -> T2;

    fn translate_run(&self, Vec<T2>) -> BinaryHeap<Vec<T1>>;
}

///Defines the approximation of `Automata`
pub trait Approximation<T, O> {
    fn approximation(&self, &T) -> Result<(O, T), String>;
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One,
      S: Clone + ApproximationStrategy<PushDown<A>, PushDown<B>,
        automata::Transition<PushDown<A>, PushDownInstruction<A>, T, W>,
        automata::Transition<PushDown<B>, PushDownInstruction<B>, T, W>>>
      Approximation<S, PushDownAutomaton<B, T, W>> for PushDownAutomaton<A, T, W>
      where W : Add<Output = W>{

    fn approximation(&self, strati : &S) -> Result<(PushDownAutomaton<B, T, W>, S), String>{
        let mut strat = strati.clone();
        let initial = strat.approximate_initial(self.initial.clone());
        
        let mut transitions = Vec::new();

        for (k, value) in self.transitions.clone(){
            if !(k == self.initial.empty){
                for t in &value{
                    let b = strat.approximate_transition(t.clone());
                    transitions.push(b);
                }
            }
        }
        Ok((PushDownAutomaton::new(
            transitions,
            initial,
        ), strat))
    }
}

impl <A: Ord + PartialEq + Debug + Clone + Hash,
      B: Ord + PartialEq + Debug + Clone + Hash,
      T: Eq + Clone +Hash,
      W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One,
      S: Clone + ApproximationStrategy<TreeStack<A>, PushDown<B>,
        automata::Transition<TreeStack<A>,TreeStackInstruction<A>, T, W>,
        automata::Transition<PushDown<B>, PushDownInstruction<B>, T, W>>>
      Approximation<S, PushDownAutomaton<B, T, W>> for TreeStackAutomaton<A, T, W>
      where W : Add<Output = W>{

    fn approximation(&self, strati : &S) -> Result<(PushDownAutomaton<B, T, W>, S), String>{
        let mut strat = strati.clone();
        let initial1 = strat.approximate_initial(self.initial.clone());
        let i = self.initial.tree.get(&Vec::new()).unwrap();
        let mut fina = initial1.empty.clone();

        let mut transitions = Vec::new();

        for (_, value) in self.transitions.clone(){
            for t in &value{
                match t.instruction{
                    TreeStackInstruction::Down { ref old_val, .. }=>{
                        let b = strat.approximate_transition(t.clone());

                        if *old_val == *i{
                            match b.instruction{
                                PushDownInstruction::Replace {ref current_val, ref new_val} =>{
                                    fina = new_val[0].clone();
                                    transitions.push(automata::Transition {
                                        _dummy: PhantomData,
                                        word: b.word.clone(),
                                        weight: b.weight.clone(),
                                        instruction: PushDownInstruction::Replace {
                                            current_val: current_val.clone(),
                                            new_val: Vec::new(),
                                        }
                                    })
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
        Ok((PushDownAutomaton::new(
            transitions,
            initial2
        ),strat))
    }
}
