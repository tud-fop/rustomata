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
    pub qi: usize,
    pub qf: usize,
    pub arcs: Vec<IntMap<(usize, W)>>,
    pub labels: Rc<HashIntegeriser<T>>,
}

impl<T, W> FiniteAutomaton<T, W> where T: Eq + Hash {
    pub fn to_unweighted(self) -> FiniteAutomaton<T, ()> {
        let FiniteAutomaton{ qi, qf, arcs, labels } = self;

        FiniteAutomaton {
            qi,
            qf,
            arcs: arcs.into_iter().map(|m| m.into_iter().map(|(s, (q, _))| (s, (q, ()))).collect()).collect(),
            labels
        }
    }
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
        qfinals: Q,
    ) -> Self
    where
        Q: Eq + Hash + Clone,
    {
        let mut arcmap = Vec::new();
        let mut states = HashIntegeriser::new();
        let mut labels = HashIntegeriser::new();

        let qi = states.integerise(qinitial);

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

        FiniteAutomaton {
            qi,
            qf: states.integerise(qfinals),
            arcs: arcmap,
            labels: Rc::new(labels),
        }
    }

    /// Creates a new deterministic FSA from integerized transitions.
    pub fn from_integerized(
        arcv: Vec<Transition<StateInstruction<usize>, usize, W>>,
        qi: usize,
        qf: usize,
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
            qi,
            qf,
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
        let mut arcs: Vec<IntMap<(usize, W)>> = Vec::new();
        let mut stack = vec![(self.qi, other.qi)];
        
        let qi = new_states.integerise((self.qi, other.qi));

        while let Some((from, from_)) = stack.pop() { 
            for (label, &(to, weight)) in self.arcs.get(from).unwrap_or(&IntMap::default()) {
                if let Some(&(to_, _)) = other.arcs.get(from_).and_then(|m| m.get(label)) {
                    let from_i = new_states.find_key(&(from, from_)).unwrap();
                    let target_i = if let Some(i) = new_states.find_key(&(to, to_)) {
                        i
                    } else {
                        stack.push((to, to_));
                        new_states.integerise((to, to_))
                    };
                    vec_entry(&mut arcs, from_i).insert(*label, (target_i, weight));
                }
            }
        }

        FiniteAutomaton {
            qi,
            qf: new_states.integerise((self.qf, other.qf)),
            arcs,
            labels: Rc::clone(&self.labels)
        }
    }

    /// Creates an `Iterator` over the language accepted by an FSA.
    pub fn generate(self, beamwidth: Capacity) -> impl Iterator<Item = Vec<T>>
    where 
        W: Copy + Mul<Output=W> + One + Zero + Ord
    {
        let heuristics = self.heuristics();

        let FiniteAutomaton {
            qi,
            qf,
            arcs,
            labels,
        } = self;

        let initial_agenda = vec![
            WeightedSearchItem(
                (qi, Vec::new(), W::one()),
                *heuristics.get(&qi).unwrap()
            ),
        ];

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
            .filter(move |&WeightedSearchItem((ref q, _, _), _)| q == &qf)
            .map(move |WeightedSearchItem((_, wi, _), _)| {
                wi.into_iter()
                    .map(|i| labels.find_value(i).unwrap())
                    .cloned()
                    .collect()
            })
    }

    fn heuristics(&self) -> IntMap<W>
    where
        W: Ord + One + Mul<Output=W> + Copy
    {
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
            vec![WeightedSearchItem(self.qf, W::one())],
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
                                       usize,
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
            &self.qi,
            &self.qf,
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
        let (qi, qf, arcs, labels) = SerializerRepresentation::deserialize(d)?;

        Ok(FiniteAutomaton {
            qi,
            qf,
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
            "initial: {}, final: {}\n{}",
            &self.qi,
            &self.qf,
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
        self.qi
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
        c.word.is_empty() && c.storage == self.qf
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
        self.qi
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use util::reverse::Reverse;


    #[test]
    fn intersection () {
        let fsa = example_fsa();
        let fsa_ = example_fsa().to_unweighted();
        
        let intersection1 = fsa.intersect(&fsa_);

        assert_eq!(intersection1.qi, example_fsa().qi);
        assert_eq!(intersection1.qf, example_fsa().qf);
        assert_eq!(intersection1.arcs, example_fsa().arcs);

        let intersection2 = fsa.intersect(&example_fsa2());

        assert_eq!(intersection2.qi, 0);
        assert_eq!(intersection2.qf, 2);
        assert_eq!(
            intersection2.arcs,
            vec![
                vec![(0, (1, 1.into()))].into_iter().collect(),
                vec![(1, (2, 1.into()))].into_iter().collect()
            ]
        );
    }

    fn example_fsa2 () -> FiniteAutomaton<char, ()> {
        FiniteAutomaton {
            qi: 0,
            qf: 0,
            arcs: vec![
                vec![(0, (1, ()))].into_iter().collect(),
                vec![(1, (0, ()))].into_iter().collect(),
            ],
            labels: Rc::clone(&example_fsa().labels)
        }
    } 

    fn example_fsa () -> FiniteAutomaton<char, Reverse<usize>> {
        let mut int = HashIntegeriser::new();
        int.integerise('a');
        int.integerise('b');

        FiniteAutomaton {
            qi: 0,
            qf: 1,
            arcs: vec![
                vec![(0, (0, 1.into())), (1, (1, 1.into()))].into_iter().collect(),
                vec![(1, (1, 1.into()))].into_iter().collect(),
            ],
            labels: Rc::new(int)
        }
    }
}