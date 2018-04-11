use integeriser::{HashIntegeriser, Integeriser};
use std::rc::Rc;
use std::hash::Hash;
use num_traits::{One, Zero};
use util::agenda::Capacity;
use util::{vec_entry, IntMap};
use util::search::{Search, WeightedSearchItem};
use recognisable::Instruction;

use std::ops::{Mul};

/// The instruction for state transitions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StateInstruction<Q>(pub Q, pub Q);

impl<Q> Instruction for StateInstruction<Q>
where
    Q: Clone + PartialEq,
{
    type Storage = Q;
    fn apply(&self, from: Q) -> Vec<Q> {
        if self.0 == from {
            vec![self.1.clone()]
        } else {
            vec![]
        }
    }
}

/// A deterministic finite state automaton.
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
    pub fn new<Q>(
        arcs: Vec<Transition<StateInstruction<Q>, T, W>>,
        qinitial: Q,
        qfinals: Vec<Q>,
    ) -> Self
    where
        Q: Eq + Hash + Clone,
    {
        let mut arcmap = Vec::new();
        let mut states = HashIntegeriser::new();
        let mut labels = HashIntegeriser::new();

        let initial = states.integerise(qinitial);

        for Transition {
            mut word,
            instruction: StateInstruction(from, to),
            weight,
        } in arcs
        {
            debug_assert!(word.len() == 1);
            vec_entry::<IntMap<(usize, W)>>(&mut arcmap, states.integerise(from))
                .insert(labels.integerise(word.remove(0)), (
                    states.integerise(to),
                    weight,
                ));
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
        arcv: Vec<Transition<StateInstruction<usize>, usize, W>>,
        initial: usize,
        finals: Vec<usize>,
        labels: Rc<HashIntegeriser<T>>,
    ) -> Self {
        let mut arcs = Vec::new();
        for Transition {
            mut word,
            weight,
            instruction: StateInstruction(from, to),
        } in arcv
        {
            debug_assert!(word.len() == 1);
            vec_entry::<IntMap<(usize, W)>>(&mut arcs, from).insert(word.remove(0), (to, weight));
        }

        FiniteAutomaton {
            arcs,
            initial,
            finals,
            labels,
        }
    }
}

impl<T, W> FiniteAutomaton<T, W>
where
    T: Eq + Hash + Clone,
{
    /// Constructs the intersection of two deterministic `FiniteAutomata`.
    pub fn intersect(&self, other: &FiniteAutomaton<T, ()>) -> Self where W: Copy {
        let mut new_states = HashIntegeriser::new();

        let mut initial_arcs = Vec::new();
        for (label, &(to, weight)) in self.arcs.get(self.initial).unwrap_or(&IntMap::default()) {
            if let Some(&(to_, _)) = other.arcs.get(other.initial).and_then(|m| m.get(label)) {
                initial_arcs.push(
                    WeightedSearchItem(
                        (self.initial, other.initial, *label, to, to_), weight)
                    );
            }
        }

        let intersect_arcs = Search::unweighted(initial_arcs, |&WeightedSearchItem((_, _, _, sto, oto), _)| {
            let mut successors = Vec::new();
            for (label, &(to, weight)) in self.arcs.get(sto).unwrap_or(&IntMap::default()) {
                if let Some(&(to_, _)) = other.arcs.get(oto).and_then(|m| m.get(label)) {
                    successors.push(
                        WeightedSearchItem((sto, oto, *label, to, to_), weight)
                    );
                }
            }
            successors
        }).uniques()
            .map(|WeightedSearchItem((from1, from2, label, to1, to2), weight)| {
                Transition {
                    instruction: StateInstruction(
                        new_states.integerise((from1, from2)),
                        new_states.integerise((to1, to2)),
                    ),
                    word: vec![label],
                    weight,
                }
            })
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
    pub fn generate(self, beamwidth: Capacity) -> impl Iterator<Item = Vec<T>> where W: Copy + Mul<Output=W> + One + Zero + Ord {
        let heuristics = self.heuristics();

        let FiniteAutomaton {
            initial,
            finals,
            arcs,
            labels,
        } = self;

        let initial_agenda = if !finals.is_empty() {
            vec![
                WeightedSearchItem(
                    (initial, Vec::new(), W::one()),
                    *heuristics.get(&initial).unwrap()
                ),
            ]
        } else {
            Vec::new()
        };

        Box::new(
            Search::weighted(initial_agenda, move |&WeightedSearchItem((q,
                                       ref word,
                                       weight),
                                      _)| {
                let mut successors = Vec::new();

                for (label, &(to, w)) in arcs.get(q).unwrap_or(&IntMap::default()) {
                    let mut word_: Vec<usize> = word.clone();
                    word_.push(label.clone());

                    let successorweight = weight * w;
                    let priority = *heuristics.get(&to).unwrap_or(&W::zero()) *
                        successorweight;

                    if !priority.is_zero() {
                        successors.push(WeightedSearchItem(
                            (to, word_, successorweight),
                            priority,
                        ))
                    }
                }
                successors
            }).beam(beamwidth)
                .filter(move |&WeightedSearchItem((ref q, _, _), _)| {
                    finals.contains(q)
                })
                .map(move |WeightedSearchItem((_, wi, _), _)| {
                    wi.into_iter()
                        .map(|i| labels.find_value(i).unwrap())
                        .cloned()
                        .collect()
                }),
        )
    }

    fn heuristics(&self) -> IntMap<W> where W: Ord + One + Mul<Output=W> + Copy {
        // index of first vector are states,
        // the second is unordered and holds all transitions targeting the state
        // without alphabet symbols
        let mut bwtrans: Vec<Vec<(usize, W)>> = Vec::new();
        for (from, arcs_from) in self.arcs.iter().enumerate() {
            for &(to, weight) in arcs_from.values() {
                vec_entry(&mut bwtrans, to).push((from, weight));
            }
        }

        Search::weighted(
            self.finals.iter().map(|q| {
                WeightedSearchItem(*q, W::one())
            }),
            move |&WeightedSearchItem(to, w)| if let Some(arcs_to) = bwtrans.get(to) {
                arcs_to
                    .iter()
                    .map(|&(from, w_)| WeightedSearchItem(from, w * w_))
                    .collect()
            } else {
                Vec::new()
            },
        ).uniques()
            .map(|WeightedSearchItem(q, w)| (q, w))
            .collect()
    }

    pub fn size(&self) -> usize {
        self.arcs.iter().flat_map(|map| map.values()).count()
    }

    pub fn get_integeriser(&self) -> Rc<HashIntegeriser<T>> {
        Rc::clone(&self.labels)
    }
}

use std::borrow::Borrow;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
type SerializerRepresentation<T, W> = (usize,
                                       Vec<usize>,
                                       Vec<IntMap<(usize, W)>>,
                                       HashIntegeriser<T>);

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
use recognisable::automaton::Automaton;
use recognisable::Transition;
use recognisable::Configuration;
use std::collections::{BinaryHeap, HashMap};
use recognisable::Item;
impl<T, W> Automaton<T, W> for FiniteAutomaton<T, W>
where
    T: Clone + Eq + Hash + Ord,
    W: Copy + One + MulAssign + Ord,
{
    type Key = usize;
    type I = StateInstruction<usize>;
    type IInt = StateInstruction<usize>;
    type TInt = usize;

    fn from_transitions<It>(_: It, _: <Self::I as Instruction>::Storage) -> Self
    where
        It: IntoIterator<Item = Transition<Self::I, T, W>>,
    {
        panic!("not implemented")
    }

    /// Returns a boxed `Iterator` over the `Transitions` of this `Automaton`.
    fn transitions<'a>(&'a self) -> Box<Iterator<Item = Transition<Self::I, T, W>> + 'a> {
        let mut v = Vec::new();
        for (from, arcs_from) in self.arcs.iter().enumerate() {
            for (isym, &(to, weight)) in arcs_from {
                v.push(Transition {
                    word: vec![self.labels.find_value(*isym).unwrap().clone()],
                    weight,
                    instruction: StateInstruction(from, to),
                })
            }
        }

        Box::new(v.into_iter())
    }

    fn initial(&self) -> <Self::I as Instruction>::Storage {
        self.initial
    }

    fn item_map(
        &self,
        i: &Item<usize, StateInstruction<usize>, usize, W>,
    ) -> Item<usize, StateInstruction<usize>, T, W> {
        let &(Configuration {
                  ref word,
                  weight,
                  storage,
              },
              ref pd) = i;

        (
            Configuration {
                word: word.iter()
                    .map(|i| self.labels.find_value(*i).unwrap())
                    .cloned()
                    .collect(),
                weight,
                storage,
            },
            pd.map(&mut |&Transition {
                      ref word,
                      instruction,
                      weight,
                  }| {
                Transition {
                    word: word.iter()
                        .map(|i| self.labels.find_value(*i).unwrap())
                        .cloned()
                        .collect(),
                    instruction,
                    weight,
                }
            }),
        )
    }

    fn terminal_to_int(&self, t: &T) -> Option<usize> {
        self.labels.find_key(t)
    }

    fn extract_key(
        c: &Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>,
    ) -> &Self::Key {
        &c.storage
    }

    fn is_terminal(
        &self,
        c: &Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>,
    ) -> bool {
        c.word.is_empty() && self.finals.contains(&c.storage)
    }

    fn transition_map(
        &self,
    ) -> Rc<HashMap<usize, BinaryHeap<Transition<StateInstruction<usize>, usize, W>>>> {
        Rc::new(
            self.arcs
                .iter()
                .enumerate()
                .map(|(from, arcs_from)| {
                    (
                        from,
                        arcs_from
                            .iter()
                            .map(|(isymbol, &(to, weight))| {
                                Transition {
                                    word: vec![*isymbol],
                                    weight,
                                    instruction: StateInstruction(from, to),
                                }
                            })
                            .collect(),
                    )
                })
                .collect(),
        )
    }

    fn initial_int(&self) -> <Self::IInt as Instruction>::Storage {
        self.initial
    }
}
