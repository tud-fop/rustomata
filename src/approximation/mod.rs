use std::collections::{BinaryHeap, BTreeMap};
use std::hash::Hash;
use std::fmt::Debug;
use std::ops::{AddAssign, Mul, Div};
use num_traits::{Zero, One};

pub mod equivalence_classes;
pub mod relabel;
pub mod ptk;
pub mod tts;

pub mod cli;

use recognisable::{Instruction, Transition};
use recognisable::automaton::Automaton;

use push_down_automaton::{PushDown, PushDownAutomaton, PushDownInstruction};
use tree_stack_automaton::{TreeStackAutomaton, TreeStackInstruction};

use self::relabel::*;
use self::ptk::*;
use self::tts::*;

/// Object defining the strategies used for `approximation`
pub trait ApproximationStrategy
    where Self::I1: Instruction,
          Self::I2: Instruction,
{
    type I1;
    type I2;

    fn approximate_storage(&self, <Self::I1 as Instruction>::Storage) -> <Self::I2 as Instruction>::Storage;

    fn approximate_instruction(&self, &Self::I1) -> Self::I2;
}

pub struct ApproximationInstance<Strategy, T, W>
    where Strategy: ApproximationStrategy,
{
    reverse_transition_map: BTreeMap<Transition<Strategy::I2, T, W>, Vec<Transition<Strategy::I1, T, W>>>,
    strategy: Strategy,
}

/// An instance of an ApproximationStrategy that remembers the approximated transitions.
impl<Strategy, T, W> ApproximationInstance<Strategy, T, W>
    where Strategy: ApproximationStrategy,
          Strategy::I2: Clone + Eq,
          Strategy::I1: Clone + Eq,
          T: Clone + Eq,
          W: Clone + Ord,
{
    pub fn new(strategy: Strategy) -> Self {
        ApproximationInstance {
            reverse_transition_map: BTreeMap::new(),
            strategy: strategy,
        }
    }

    pub fn approximate_storage(&self, s1: <Strategy::I1 as Instruction>::Storage)
                               -> <Strategy::I2 as Instruction>::Storage
    {
        self.strategy.approximate_storage(s1)
    }

    pub fn approximate_instruction(&self, i1: &Strategy::I1) -> Strategy::I2 {
        self.strategy.approximate_instruction(i1)
    }

    pub fn approximate_transition(&mut self, t1: Transition<Strategy::I1, T, W>)
                                  -> Transition<Strategy::I2, T, W>
    {
        let t2 = Transition { word: t1.word.clone(),
                              instruction: self.approximate_instruction(&t1.instruction),
                              weight: t1.weight.clone(), };
        self.reverse_transition_map.entry(t2.clone()).or_insert(Vec::new()).push(t1);
        t2
    }

    pub fn unapproximate_transition(&self, t2: &Transition<Strategy::I2, T, W>)
                                    -> Vec<Transition<Strategy::I1, T, W>>
    {
        match self.reverse_transition_map.get(t2) {
            None => Vec::new(),
            Some(v) => v.clone(),
        }
    }

    pub fn unapproximate_run(&self, run2: Vec<Transition<Strategy::I2, T, W>>)
                             -> BinaryHeap<Vec<Transition<Strategy::I1, T, W>>>
    {
        let f = |h: BinaryHeap<Vec<_>>, ts1: Vec<_>| {
            let new_runs: Vec<Vec<_>> =
                h.into_iter()
                .flat_map( |run: Vec<_>| -> Vec<Vec<_>>{
                    let push_transition = |t1: &Transition<_, _, _>| {
                        let mut new_run = run.clone();
                        new_run.push(t1.clone());
                        new_run
                    };
                    ts1.iter().map(push_transition).collect()
                } ).collect();
            BinaryHeap::from(new_runs)
        };

        let initial_heap = BinaryHeap::from(vec![Vec::new()]);
        run2.iter().map(|t2| self.unapproximate_transition(t2)).fold(initial_heap, f)
    }

}

/// Defines the approximation of `Automata`
pub trait Approximation<Strategy, Output, T, W>
    where Strategy: ApproximationStrategy,
{
    fn approximation(&self, Strategy) -> Result<(Output, ApproximationInstance<Strategy, T, W>), String>;
}

impl<S, A, B, T, W> Approximation<S, PushDownAutomaton<B, T, W>, T, W> for PushDownAutomaton<A, T, W>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          B: Ord + PartialEq + Debug + Clone + Hash,
          T: Clone + Debug + Eq + Hash + PartialOrd,
          W: Ord + Eq + Clone + Copy + Debug + AddAssign + Mul<Output = W> + Div<Output = W> + Zero + One,
          S: ApproximationStrategy<I1=PushDownInstruction<A>, I2=PushDownInstruction<B>>
{
    fn approximation(&self, strati: S)
                     -> Result<(PushDownAutomaton<B, T, W>, ApproximationInstance<S, T, W>), String>{
        let mut strat = ApproximationInstance::new(strati);
        let initial = strat.approximate_storage(self.initial());

        let mut transitions = Vec::new();

        for (k, value) in self.transitions().iter() {
            if !(*k == *self.initial().empty()){
                for t in value {
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

impl<S, A, B, T, W> Approximation<S, PushDownAutomaton<B, T, W>, T, W> for TreeStackAutomaton<A, T, W>
    where A: Ord + PartialEq + Debug + Clone + Hash,
          B: Ord + PartialEq + Debug + Clone + Hash,
          T: Eq + Clone +Hash,
          W: Ord + Eq + Clone + AddAssign + Mul<Output = W> + Div<Output = W> + Zero + One,
          S: ApproximationStrategy<I1=TreeStackInstruction<A>, I2=PushDownInstruction<B>>,
{
    fn approximation(&self, strati: S)
                     -> Result<(PushDownAutomaton<B, T, W>, ApproximationInstance<S, T, W>), String>{
        let mut strat = ApproximationInstance::new(strati);
        let initial1 = strat.approximate_storage(self.initial());
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
