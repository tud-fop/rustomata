use std::collections::{BinaryHeap, HashMap};
use integeriser::{HashIntegeriser, Integeriser};
use log_domain::LogDomain;
use std::rc::Rc;
use std::hash::Hash;
use num_traits::{One, Zero};
use super::dka::heuristics;
use util::agenda::Capacity;
use util::{vec_entry, IntMap};

/// A transition of a deterministic finite automaton.
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct FiniteArc<Q, T, W> {
    pub from: Q,
    pub to: Q,
    pub label: T,
    pub weight: W,
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

use recognisable::{Search, UniqueSearch};
impl<T> FiniteAutomaton<T, LogDomain<f64>>
where
    T: Eq + Hash + Clone,
{
    /// Computes the Hadamard product of two deterministic `FiniteAutomata`.
    fn intersect<W>(&self, filter: FiniteAutomaton<T, W>) -> Self {
        let &FiniteAutomaton {
            initial,
            ref finals,
            ref arcs,
            ref labels,
        } = self;
        let FiniteAutomaton {
            initial: finitial,
            finals: ffinals,
            arcs: farcs,
            ..
        } = filter;

        let mut new_states = HashIntegeriser::new();

        let mut initial_arcs: Vec<FiniteArc<(usize, usize), usize, LogDomain<f64>>> = Vec::new();
        for (label, &(to, weight)) in arcs.get(initial).unwrap_or(&IntMap::default()) {
            if let Some(&(to_, _)) = farcs.get(finitial).and_then(|m| m.get(label)) {
                initial_arcs.push(FiniteArc {
                    from: (initial, filter.initial),
                    to: (to, to_),
                    label: *label,
                    weight,
                });
            }
        }

        let intersect_arcs = Search::unweighted(
            initial_arcs,
            Box::new(move |&FiniteArc { to: (sto, oto), .. }| {
                let mut successors = Vec::new();
                for (label, &(to, weight)) in arcs.get(sto).unwrap_or(&IntMap::default()) {
                    if let Some(&(to_, _)) = farcs.get(oto).and_then(|m| m.get(label)) {
                        successors.push(FiniteArc {
                            from: (sto, oto),
                            to: (to, to_),
                            label: *label,
                            weight,
                        });
                    }
                }
                successors
            }),
        ).map(
            |FiniteArc {
                 from,
                 to,
                 label,
                 weight,
             }| {
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
        for qf in finals {
            for qf_ in &ffinals {
                if let Some(i) = new_states.find_key(&(*qf, *qf_)) {
                    intersect_finals.push(i);
                }
            }
        }

        FiniteAutomaton::from_integerized(
            intersect_arcs,
            new_states.integerise((initial, finitial)),
            intersect_finals,
            Rc::clone(labels),
        )
    }

    /// Creates an `Iterator` over the language accepted by an FSA.
    fn generate<'a>(self, beam: Capacity) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        let FiniteAutomaton {
            initial,
            finals,
            arcs,
            labels,
        } = self;

        let heuristics = {
            let mut backwards_transitions = HashMap::new();
            for (from, arcs_from) in arcs.iter().enumerate() {
                for &(to, weight) in arcs_from.values() {
                    backwards_transitions
                        .entry(to)
                        .or_insert_with(BinaryHeap::new)
                        .push((weight, from));
                }
            }
            heuristics(backwards_transitions, finals.as_slice())
        };

        Box::new(
            UniqueSearch::weighted(
                vec![(initial, vec![], LogDomain::one())],
                Box::new(move |&(q, ref word, weight)| {
                    let mut successors = Vec::new();
                    for (label, &(to, w)) in arcs.get(q).unwrap_or(&IntMap::default()) {
                        let mut word_ = word.clone();
                        word_.push(label.clone());
                        successors.push((to, word_, weight * w))
                    }
                    successors
                }),
                Box::new(move |&(ref q, _, w)| {
                    (*heuristics.get(q).unwrap_or(&LogDomain::zero()) * w).pow(-1.0)
                }),
                beam,
            ).filter(move |&(ref q, _, _)| finals.contains(q))
                .map(move |(_, wi, _)| {
                    wi.into_iter()
                        .map(|i| labels.find_value(i).unwrap())
                        .cloned()
                        .collect()
                }),
        )
    }
}

use super::GeneratorAutomaton;

impl<T> GeneratorAutomaton<T> for FiniteAutomaton<T, LogDomain<f64>>
where
    T: Eq + Hash + Clone,
{
    fn get_integeriser(&self) -> Rc<HashIntegeriser<T>> {
        Rc::clone(&self.labels)
    }

    fn generate<'a>(
        &self,
        filter: FiniteAutomaton<T, ()>,
        beam: Capacity,
    ) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        let intersection = self.intersect(filter);
        intersection.generate(beam)
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
        let &FiniteAutomaton {
            ref initial,
            ref finals,
            ref arcs,
            ref labels,
        } = self;

        (
            initial,
            finals,
            arcs,
            Borrow::<HashIntegeriser<T>>::borrow(labels),
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
        let &FiniteAutomaton {
            ref arcs,
            ref initial,
            ref finals,
            ref labels,
        } = self;

        let mut buffer = String::new();
        for (from, arcs_from) in arcs.iter().enumerate() {
            for (label, &(ref to, ref weight)) in arcs_from {
                buffer.push_str(&format!(
                    "{} → [{}] {} # {}\n",
                    from,
                    labels.find_value(*label).unwrap(),
                    to,
                    weight
                ));
            }
        }
        write!(f, "initial: {}, finals: {:?}\n{}", initial, finals, &buffer)
    }
}
