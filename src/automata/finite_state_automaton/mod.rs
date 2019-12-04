extern crate bit_set;

use crate::recognisable::{automaton::Automaton, Configuration, Instruction, Item, Transition};
use crate::util::integerisable::{Integerisable1, Integerisable2};
use crate::util::push_down::Pushdown;
use integeriser::{HashIntegeriser, Integeriser};
use num_traits::One;
use std::collections::{BinaryHeap, HashMap};
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::MulAssign;
use std::rc::Rc;

use self::bit_set::BitSet;

mod from_str;

type TransitionMap<Q, T, W> = HashMap<Q, BinaryHeap<Transition<FiniteStateInstruction<Q>, T, W>>>;

#[derive(Clone, Debug)]
pub struct FiniteStateAutomaton<Q, T, W>
where
    Q: Hash + Ord,
    T: Eq + Hash,
    W: Ord,
{
    q_integeriser: HashIntegeriser<Q>,
    t_integeriser: HashIntegeriser<T>,
    transitions: Rc<TransitionMap<usize, usize, W>>,
    initial_state: usize,
    final_states: BitSet,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FiniteStateInstruction<Q> {
    source_state: Q,
    target_state: Q,
}

impl<Q: Clone + Eq + Hash> Integerisable1 for FiniteStateInstruction<Q> {
    type AInt = FiniteStateInstruction<usize>;
    type I = HashIntegeriser<Q>;

    fn integerise(&self, integeriser: &mut Self::I) -> Self::AInt {
        FiniteStateInstruction {
            source_state: integeriser.integerise(self.source_state.clone()),
            target_state: integeriser.integerise(self.target_state.clone()),
        }
    }

    fn un_integerise(v: &Self::AInt, integeriser: &Self::I) -> Self {
        FiniteStateInstruction {
            source_state: integeriser.find_value(v.source_state).unwrap().clone(),
            target_state: integeriser.find_value(v.target_state).unwrap().clone(),
        }
    }
}

impl<Q> Instruction for FiniteStateInstruction<Q>
where
    Q: Eq + Clone,
{
    type Storage = Q;

    fn apply(&self, p: Self::Storage) -> Vec<Self::Storage> {
        if self.source_state == p {
            vec![self.target_state.clone()]
        } else {
            Vec::new()
        }
    }
}

impl<Q, T, W> FiniteStateAutomaton<Q, T, W>
where
    Q: Clone + Eq + Hash + Ord,
    T: Clone + Eq + Hash + Ord,
    W: Clone + Ord,
{
    pub fn new<TransIt, FinalsIt>(transitions: TransIt, initial: Q, finals: FinalsIt) -> Self
    where
        TransIt: IntoIterator<Item = Transition<FiniteStateInstruction<Q>, T, W>>,
        FinalsIt: IntoIterator<Item = Q>,
    {
        let mut q_inter = HashIntegeriser::new();
        let mut t_inter = HashIntegeriser::new();
        let init = q_inter.integerise(initial);
        let fin = finals.into_iter().map(|q| q_inter.integerise(q)).collect();
        let mut transition_map: TransitionMap<usize, usize, W> = HashMap::new();

        for trans in transitions
            .into_iter()
            .map(|t| t.integerise(&mut t_inter, &mut q_inter))
        {
            transition_map
                .entry(trans.instruction.source_state)
                .or_insert_with(BinaryHeap::new)
                .push(trans);
        }

        FiniteStateAutomaton {
            q_integeriser: q_inter,
            t_integeriser: t_inter,
            transitions: Rc::new(transition_map),
            initial_state: init,
            final_states: fin,
        }
    }

    pub fn list_transitions<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = Transition<FiniteStateInstruction<Q>, T, W>> + 'a> {
        Box::new(self.transitions.values().flat_map(move |h| {
            h.iter().map(move |t| {
                Transition::un_integerise(t, &self.t_integeriser, &self.q_integeriser)
            })
        }))
    }
}

impl<Q, T, W> Automaton<T, W> for FiniteStateAutomaton<Q, T, W>
where
    Q: Clone + Hash + Ord,
    T: Clone + Eq + Hash + Ord,
    W: Clone + MulAssign + One + Ord,
{
    type Key = usize;
    type I = FiniteStateInstruction<Q>;
    type IInt = FiniteStateInstruction<usize>;
    type TInt = usize;

    fn from_transitions<It>(transitions: It, initial: Q) -> Self
    where
        It: IntoIterator<Item = Transition<FiniteStateInstruction<Q>, T, W>>,
    {
        FiniteStateAutomaton::new(transitions, initial, Vec::new())
    }

    fn transitions<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = Transition<FiniteStateInstruction<Q>, T, W>> + 'a> {
        self.list_transitions()
    }

    fn initial(&self) -> Q {
        self.q_integeriser
            .find_value(self.initial_state)
            .unwrap()
            .clone()
    }

    fn item_map(
        &self,
        i: &Item<usize, FiniteStateInstruction<usize>, usize, W>,
    ) -> Item<Q, FiniteStateInstruction<Q>, T, W> {
        match *i {
            Item(
                Configuration {
                    ref word,
                    ref storage,
                    ref weight,
                },
                ref pd,
            ) => {
                let pd_vec: Vec<_> = pd.clone().into();
                let pd_unint: Vec<_> = pd_vec
                    .iter()
                    .map(|t| {
                        Integerisable2::un_integerise(t, &self.t_integeriser, &self.q_integeriser)
                    })
                    .collect();
                Item(
                    Configuration {
                        word: word
                            .iter()
                            .map(|t| self.t_integeriser.find_value(*t).unwrap().clone())
                            .collect(),
                        storage: self.q_integeriser.find_value(*storage).unwrap().clone(),
                        weight: weight.clone(),
                    },
                    Pushdown::from(pd_unint.as_slice()),
                )
            }
        }
    }

    fn terminal_to_int(&self, t: &T) -> Option<usize> {
        self.t_integeriser.find_key(t)
    }

    fn extract_key(c: &Configuration<usize, usize, W>) -> &usize {
        &c.storage
    }

    fn is_terminal(&self, c: &Configuration<usize, usize, W>) -> bool {
        c.word.is_empty() && self.final_states.contains(c.storage)
    }

    fn transition_map(&self) -> Rc<TransitionMap<usize, usize, W>> {
        self.transitions.clone()
    }

    fn initial_int(&self) -> usize {
        self.initial_state
    }
}

impl<Q> Display for FiniteStateInstruction<Q>
where
    Q: Display,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} → {}", self.source_state, self.target_state)
    }
}

impl<Q, T, W> Display for FiniteStateAutomaton<Q, T, W>
where
    Q: Clone + Display + Hash + Ord,
    T: Clone + Debug + Display + Eq + Hash + Ord,
    W: Clone + Display + Ord,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut buffer = "".to_string();
        buffer.push_str(&format!("initial: {}\n", self.initial_state));

        buffer.push_str("final: [");
        let mut first = true;
        for i in 0..self.final_states.len() {
            if self.final_states.contains(i) {
                if first {
                    buffer.push_str(", ");
                    first = false;
                }
                buffer.push_str(&format!("\"{}\"", i));
            }
        }
        buffer.push_str("]\n\n");
        for t in self.list_transitions() {
            buffer.push_str(&format!("{}\n", t));
        }

        write!(f, "{}", buffer)
    }
}
