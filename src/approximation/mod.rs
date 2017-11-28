use std::collections::{BinaryHeap};
use std::hash::Hash;
use std::fmt::Debug;
use std::ops::{Add, Mul, Div};
use num_traits::{Zero, One};

pub mod equivalence_classes;
pub mod relabel;
pub mod ptk;
pub mod tts;

pub mod cli;

use automata::{Instruction, Transition};

use push_down_automaton::{PushDown, PushDownAutomaton, PushDownInstruction};
use tree_stack_automaton::{TreeStackAutomaton, TreeStackInstruction};

use self::relabel::*;
use self::ptk::*;
use self::tts::*;

type PushDownTransitionSequence<A, T, W> = Vec<Transition<PushDownInstruction<A>, T, W>>;
type TreeStackTransitionSequence<A, T, W> = Vec<Transition<TreeStackInstruction<A>, T, W>>;

/// Object defining the strategies used for `approximation`
pub trait ApproximationStrategy<I1, I2, T, W>
    where I1: Instruction,
          I2: Instruction,
{
    fn approximate_initial(&self, I1::Storage) -> I2::Storage;

    fn add_transitions(&mut self, &Transition<I1, T, W>, &Transition<I2, T, W>);

    fn approximate_transition(&mut self, Transition<I1, T, W>) -> Transition<I2, T, W>;

    fn translate_run(&self, Vec<Transition<I2, T, W>>) -> BinaryHeap<Vec<Transition<I1, T, W>>>;
}

///Defines the approximation of `Automata`
pub trait Approximation<T, O> {
    fn approximation(&self, &T) -> Result<(O, T), String>;
}

impl<S, A, B, T, W> Approximation<S, PushDownAutomaton<B, T, W>> for PushDownAutomaton<A, T, W>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          B: Ord + PartialEq + Debug + Clone + Hash,
          T: Eq + Clone +Hash,
          W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One,
          S: Clone + ApproximationStrategy<PushDownInstruction<A>, PushDownInstruction<B>, T, W>
{
    fn approximation(&self, strati : &S) -> Result<(PushDownAutomaton<B, T, W>, S), String>{
        let mut strat = strati.clone();
        let initial = strat.approximate_initial(self.initial.clone());

        let mut transitions = Vec::new();

        for (k, value) in self.transitions.clone(){
            if !(k == *self.initial.empty()){
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

impl<S, A, B, T, W> Approximation<S, PushDownAutomaton<B, T, W>> for TreeStackAutomaton<A, T, W>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          B: Ord + PartialEq + Debug + Clone + Hash,
          T: Eq + Clone +Hash,
          W: Ord + Eq + Clone + Add<Output=W> + Mul<Output = W> + Div<Output = W> + Zero + One,
          S: Clone + ApproximationStrategy<TreeStackInstruction<A>, PushDownInstruction<B>, T, W>,
{
    fn approximation(&self, strati: &S) -> Result<(PushDownAutomaton<B, T, W>, S), String>{
        let mut strat = strati.clone();
        let initial1 = strat.approximate_initial(self.initial());
        let i = self.initial().current_symbol().clone();
        let mut fina = initial1.empty().clone();

        let mut transitions = Vec::new();

        for (_, value) in self.transitions() {
            for t in value {
                match t.instruction{
                    TreeStackInstruction::Down { ref old_val, .. }=>{
                        let b = strat.approximate_transition(t.clone());

                        if *old_val == i {
                            match b.instruction {
                                PushDownInstruction::Replace {ref current_val, ref new_val} => {
                                    fina = new_val[0].clone();
                                    transitions.push(Transition {
                                        word: b.word.clone(),
                                        weight: b.weight.clone(),
                                        instruction: PushDownInstruction::Replace {
                                            current_val: current_val.clone(),
                                            new_val: Vec::new(),
                                        }
                                    })
                                },
                                _ => {
                                    transitions.push(b.clone());
                                },
                            }
                        }else{
                            transitions.push(b.clone());
                        }
                    },
                    _ => {
                        let b = strat.approximate_transition(t.clone());
                        transitions.push(b);
                    },
                }

            }
        }
        let initial2 = PushDown::new(initial1.empty().clone(), fina);
        Ok((PushDownAutomaton::new(
            transitions,
            initial2
        ),strat))
    }
}
