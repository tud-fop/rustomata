use integeriser::{HashIntegeriser, Integeriser};
use log_domain::LogDomain;
use std::rc::Rc;
use std::hash::Hash;
use num_traits::{One, Zero};
use util::agenda::Capacity;
use util::{vec_entry, IntMap};
use recognisable::{Search, WeightedSearchItem};


/// A transition of a deterministic finite automaton.
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct FiniteArc<Q, T, W> {
    pub from: Q,
    pub to: Q,
    pub label: T,
    pub weight: W,
}

use Instruction;
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StateInstruction<Q>(Q, Q);

impl<Q> Instruction for StateInstruction<Q> where Q: Clone + PartialEq {
    type Storage = Q;
    fn apply(&self, from: Q) -> Vec<Q> {
        if self.0 == from {
            vec![self.1.clone()]
        } else {
            vec![]
        }
    }
}

/// A deterministic finite automaton.
#[derive(Clone, Debug)]
pub struct FiniteAutomaton<T, W>
where
    T: Eq + Hash,
{
    pub initial: usize,
    pub finals: Vec<usize>,
    pub arcs: Vec<IntMap<(usize, W)>>,
    pub labels: Rc<HashIntegeriser<T>>,
}

impl<T, W> FiniteAutomaton<T, W>
where
    T: Eq + Hash + Clone,
    W: Copy,
{
    /// Creates a new deterministic finite automaton from a list
    /// of transitions.
    /// If there are multiple transitions from the same state and with the same
    /// label, the last dominates.
    pub fn new<Q>(arcs: Vec<FiniteArc<Q, T, W>>, qinitial: Q, qfinals: Vec<Q>) -> Self
    where
        Q: Eq + Hash + Clone,
    {
        let mut arcmap = Vec::new();
        let mut states = HashIntegeriser::new();
        let mut labels = HashIntegeriser::new();

        let initial = states.integerise(qinitial);

        for FiniteArc {
            from,
            to,
            label,
            weight,
        } in arcs
        {
            vec_entry::<IntMap<(usize, W)>>(&mut arcmap, states.integerise(from))
                .insert(labels.integerise(label), (states.integerise(to), weight));
        }

        let finals = qfinals
            .into_iter()
            .filter_map(|q| states.find_key(&q))
            .collect();

        FiniteAutomaton {
            initial,
            finals,
            arcs: arcmap,
            labels: Rc::new(labels),
        }
    }

    /// Creates a new deterministic FSA from intergerized transitions.
    pub fn from_integerized(
        arcv: Vec<FiniteArc<usize, usize, W>>,
        initial: usize,
        finals: Vec<usize>,
        labels: Rc<HashIntegeriser<T>>,
    ) -> Self {
        let mut arcs = Vec::new();
        for FiniteArc {
            from,
            to,
            label,
            weight,
        } in arcv
        {
            vec_entry::<IntMap<(usize, W)>>(&mut arcs, from).insert(label, (to, weight));
        }

        FiniteAutomaton {
            arcs,
            initial,
            finals,
            labels,
        }
    }
}

impl<T> FiniteAutomaton<T, LogDomain<f64>>
where
    T: Eq + Hash + Clone,
{
    /// Computes the Hadamard product of two deterministic `FiniteAutomata`.
    fn intersect<W>(&self, other: &FiniteAutomaton<T, W>) -> Self {
        let mut new_states = HashIntegeriser::new();

        let mut initial_arcs: Vec<FiniteArc<(usize, usize), usize, LogDomain<f64>>> = Vec::new();
        for (label, &(to, weight)) in self.arcs.get(self.initial).unwrap_or(&IntMap::default()) {
            if let Some(&(to_, _)) = other.arcs.get(other.initial).and_then(|m| m.get(label)) {
                initial_arcs.push(FiniteArc {
                    from: (self.initial, other.initial),
                    to: (to, to_),
                    label: *label,
                    weight,
                });
            }
        }

        let intersect_arcs =
            Search::unweighted(initial_arcs, |&FiniteArc { to: (sto, oto), .. }| {
                let mut successors = Vec::new();
                for (label, &(to, weight)) in self.arcs.get(sto).unwrap_or(&IntMap::default()) {
                    if let Some(&(to_, _)) = other.arcs.get(oto).and_then(|m| m.get(label)) {
                        successors.push(FiniteArc {
                            from: (sto, oto),
                            to: (to, to_),
                            label: *label,
                            weight,
                        });
                    }
                }
                successors
            }).uniques()
                .map(
                    |FiniteArc { from, to, label, weight }| {
                        FiniteArc {
                            from: new_states.integerise(from),
                            to: new_states.integerise(to),
                            label,
                            weight,
                        }
                    },
                )
                .collect();

        let mut intersect_finals = Vec::new();
        for qf in &self.finals {
            for qf_ in &other.finals {
                if let Some(i) = new_states.find_key(&(*qf, *qf_)) {
                    intersect_finals.push(i);
                }
            }
        }

        FiniteAutomaton::from_integerized(
            intersect_arcs,
            new_states.integerise((self.initial, other.initial)),
            intersect_finals,
            Rc::clone(&self.labels),
        )
    }

    /// Creates an `Iterator` over the language accepted by an FSA.
    fn generate<'a>(self, beamwidth: Capacity) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        let heuristics = self.heuristics();
        let FiniteAutomaton {
            initial,
            finals,
            arcs,
            labels,
        } = self;

        let initial_agenda =
            if !finals.is_empty() {
                vec![(initial, Vec::new(), LogDomain::one())]
            } else {
                Vec::new()
            };

        Box::new(
            Search::weighted(
                initial_agenda,
                move |&(q, ref word, weight)| {
                    let mut successors = Vec::new();
                    for (label, &(to, w)) in arcs.get(q).unwrap_or(&IntMap::default()) {
                        let mut word_: Vec<usize> = word.clone();
                        word_.push(label.clone());
                        successors.push((to, word_, weight * w))
                    }
                    successors
                },
                Box::new(move |&(ref q, _, w)| {
                    (*heuristics.get(q).unwrap_or(&LogDomain::zero()) * w).pow(-1.0)
                }),
            ).beam(beamwidth)
                .filter(move |&(ref q, _, _)| finals.contains(q))
                .map(move |(_, wi, _)| {
                    wi.into_iter()
                        .map(|i| labels.find_value(i).unwrap())
                        .cloned()
                        .collect()
                }),
        )
    }

    fn heuristics(&self) -> IntMap<LogDomain<f64>> {
        // index of first vector are states,
        // the second is unordered and holds all transitions targeting the state
        // without alphabet symbols
        let mut bwtrans: Vec<Vec<(usize, LogDomain<f64>)>> = Vec::new();
        for (from, arcs_from) in self.arcs.iter().enumerate() {
            for &(to, weight) in arcs_from.values() {
                vec_entry(&mut bwtrans, to).push((from, weight));
            }
        }

        Search::weighted(
            self.finals
                .iter()
                .map(|q| WeightedSearchItem(*q, LogDomain::one())),
            move |&WeightedSearchItem(to, w)| {
                if let Some(arcs_to) = bwtrans.get(to) {
                    arcs_to
                        .iter()
                        .map(|&(from, w_)| WeightedSearchItem(from, w_ * w))
                        .collect()
                } else {
                    Vec::new()
                }
            },
            Box::new(|&WeightedSearchItem(_, w)| w.pow(-1.0)),
        ).uniques()
            .map(|WeightedSearchItem(q, w)| (q, w))
            .collect()
    }
}

use super::GeneratorAutomaton;

impl<T> GeneratorAutomaton<T> for FiniteAutomaton<T, LogDomain<f64>>
where
    T: Eq + Hash + Clone,
{
    fn size(&self) -> usize {
        self.arcs.iter().flat_map(|map| map.values()).count()
    }

    fn get_integeriser(&self) -> Rc<HashIntegeriser<T>> {
        Rc::clone(&self.labels)
    }

    fn intersect(&self, other: FiniteAutomaton<T, ()>) -> Self {
        self.intersect(&other)
    }

    fn generate<'a>(self, beam: Capacity) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        self.generate(beam)
    }
}

use std::borrow::Borrow;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
type SerializerRepresentation<T, W> = (
    usize,
    Vec<usize>,
    Vec<IntMap<(usize, W)>>,
    HashIntegeriser<T>,
);

impl<T, W> Serialize for FiniteAutomaton<T, W>
where
    T: Serialize + Eq + Hash,
    W: Serialize,
{
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        (
            &self.initial,
            &self.finals,
            &self.arcs,
            Borrow::<HashIntegeriser<T>>::borrow(&self.labels),
        ).serialize(s)
    }
}

impl<'de, T, W> Deserialize<'de> for FiniteAutomaton<T, W>
where
    T: Deserialize<'de> + Eq + Hash + Clone,
    W: Deserialize<'de>,
{
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let (initial, finals, arcs, labels) = SerializerRepresentation::deserialize(d)?;

        Ok(FiniteAutomaton {
            initial,
            finals,
            arcs,
            labels: Rc::new(labels),
        })
    }
}

use std::fmt::{Display, Error, Formatter};
impl<T, W> Display for FiniteAutomaton<T, W>
where
    T: Display + Hash + Eq + Clone,
    W: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let mut buffer = String::new();

        for (from, arcs_from) in self.arcs.iter().enumerate() {
            for (label, &(ref to, ref weight)) in arcs_from {
                buffer.push_str(&format!(
                    "{} → [{}] {} # {}\n",
                    from,
                    self.labels.find_value(*label).unwrap(),
                    to,
                    weight
                ));
            }
        }

        write!(
            f,
            "initial: {}, finals: {:?}\n{}",
            &self.initial,
            &self.finals,
            &buffer
        )
    }
}

use std::ops::MulAssign;
use automaton::Automaton;
use Transition;
use Configuration;
use std::collections::{HashMap, BinaryHeap};
use recognisable::Item;
impl<T, W> Automaton<T, W> for FiniteAutomaton<T, W> 
where
    T: Clone + Eq + Hash + Ord,
    W: Copy + One + MulAssign + Ord
{
    type Key = usize;
    type I = StateInstruction<usize>;
    type IInt = StateInstruction<usize>;
    type TInt = usize;

    fn from_transitions<It>(transitions: It, initial: <Self::I as Instruction>::Storage) -> Self
    where 
        It: IntoIterator<Item=Transition<Self::I, T, W>>
    {
        panic!("not implemented")
    }

    /// Returns a boxed `Iterator` over the `Transitions` of this `Automaton`.
    fn transitions<'a>(&'a self) -> Box<Iterator<Item=Transition<Self::I, T, W>> + 'a>
    {
        let mut v = Vec::new();
        for (from, arcs_from) in self.arcs.iter().enumerate() {
            for (isym, &(to, weight)) in arcs_from {
                v.push(
                    Transition{ 
                        word: vec![self.labels.find_value(*isym).unwrap().clone()],
                        weight,
                        instruction: StateInstruction(from, to)
                    }
                )
            }
        }

        Box::new(
            v.into_iter()
        )
    }

    fn initial(&self) -> <Self::I as Instruction>::Storage
    {
        self.initial
    }

    /// Maps items from the internal representation to the desired output.
    fn item_map(&self, i: &Item<usize, StateInstruction<usize>, usize, W>) -> Item<usize, StateInstruction<usize>, T, W>
    {
        let &(Configuration{ ref word, weight, storage }, ref pd) = i;

        (
            Configuration{
                word: word.iter().map(|i| self.labels.find_value(*i).unwrap()).cloned().collect(),
                weight,
                storage
            },
            pd.map(
                &mut | &Transition{ ref word, instruction, weight } |
                Transition {
                    word: word.iter().map(|i| self.labels.find_value(*i).unwrap()).cloned().collect(),
                    instruction,
                    weight
                }
            )
        )
    }

    fn terminal_to_int(&self, t: &T) -> usize {
        self.labels.find_key(t).unwrap()
    }

    fn extract_key(c: &Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>) -> &Self::Key
    {
        &c.storage
    }

    fn is_terminal(c: &Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>) -> bool
    {
        panic!("not implemented")
    }

    fn transition_map(&self) -> Rc<HashMap<usize, BinaryHeap<Transition<StateInstruction<usize>, usize, W>>>>
    {
        Rc::new(
            self.arcs.iter().enumerate().map(
                | (from, arcs_from) | {
                    (
                        from,
                        arcs_from.iter().map(
                            | (isymbol, &(to, weight)) | 
                            Transition{ 
                                word: vec![*isymbol],
                                weight,
                                instruction: StateInstruction(from, to)
                            }
                        ).collect()
                    )
                }
            ).collect()
        )
    }

    /// Returns the initial storage configuration (in its internal representation).
    fn initial_int(&self) -> <Self::IInt as Instruction>::Storage
    {
        self.initial
    }

}
