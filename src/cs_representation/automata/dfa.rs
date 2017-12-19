use std::collections::{HashMap, BinaryHeap};
use integeriser::{HashIntegeriser, Integeriser};
use log_domain::LogDomain;
use std::rc::Rc;
use std::hash::Hash;
use num_traits::{One, Zero};
use super::dka::heuristics;
use util::agenda::Capacity;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct FiniteArc<Q, T> {
    pub from: Q,
    pub to: Q,
    pub label: T,
    pub weight: LogDomain<f32>,
}

#[derive(Clone)]
pub struct FiniteAutomaton<T>
where
    T: Eq + Hash,
{
    pub initial: usize,
    pub finals: Vec<usize>,
    pub arcs: HashMap<usize, HashMap<usize, (usize, LogDomain<f32>)>>,
    pub labels: Rc<HashIntegeriser<T>>,
}

impl<T> FiniteAutomaton<T>
where
    T: Eq + Hash + Clone,
{
    pub fn new<Q>(arcs: Vec<FiniteArc<Q, T>>, qinitial: Q, qfinals: Vec<Q>) -> Self
    where
        Q: Eq + Hash + Clone,
    {
        let mut arcmap = HashMap::new();
        let mut states = HashIntegeriser::new();
        let mut labels = HashIntegeriser::new();

        let initial = states.integerise(qinitial);
        let finals = qfinals.into_iter().map(|q| states.integerise(q)).collect();

        for FiniteArc {
            from,
            to,
            label,
            weight,
        } in arcs
        {
            let arcs_from = arcmap
                .entry(states.integerise(from))
                .or_insert(HashMap::new());
            arcs_from.insert(labels.integerise(label), (states.integerise(to), weight));
        }

        FiniteAutomaton {
            initial,
            finals,
            arcs: arcmap,
            labels: Rc::new(labels),
        }
    }

    pub fn from_integerized(
        arcv: Vec<FiniteArc<usize, usize>>,
        initial: usize,
        finals: Vec<usize>,
        labels: Rc<HashIntegeriser<T>>,
    ) -> Self {
        let mut arcs = HashMap::new();
        for FiniteArc {
            from,
            to,
            label,
            weight,
        } in arcv
        {
            arcs.entry(from)
                .or_insert(HashMap::new())
                .insert(label, (to, weight));
        }

        FiniteAutomaton {
            arcs,
            initial,
            finals,
            labels,
        }
    }
}

use recognisable::{UniqueSearch, Search};
impl<T> FiniteAutomaton<T> where T: Eq + Hash + Clone {
    fn intersect(&self, filter: Self) -> Self {
        let &FiniteAutomaton{ ref initial, ref finals, ref arcs, ref labels } = self;
        let FiniteAutomaton{ initial: finitial, finals: ffinals, arcs: farcs, .. } = filter;
        
        let mut new_states = HashIntegeriser::new();
        
        let mut initial_arcs: Vec<FiniteArc<(usize, usize), usize>> = Vec::new();
        for (label, &(to, weight)) in arcs.get(initial).unwrap_or(&HashMap::new()) {
            if let Some(&(to_, _)) = farcs.get(&filter.initial).and_then(|m| m.get(label)) {
                initial_arcs.push(
                    FiniteArc{
                        from: (*initial, filter.initial),
                        to: (to, to_),
                        label: *label,
                        weight
                    }
                );
            }
        }

        let intersect_arcs = Search::unweighted(
            initial_arcs,
            Box::new(
                move | &FiniteArc{ to: (ref sto, ref oto), .. } |{
                    let mut successors = Vec::new();
                    for (label, &(to, weight)) in arcs.get(sto).unwrap_or(&HashMap::new()) {
                            if let Some(&(to_, _)) = farcs.get(oto).and_then(|m| m.get(label)) {
                                successors.push(
                                    FiniteArc{
                                        from: (*sto, *oto),
                                        to: (to, to_),
                                        label: *label,
                                        weight
                                    }
                                );
                            }
                        }
                        successors
                }
            )
        ).map(
            | FiniteArc{from, to, label, weight} |
            FiniteArc{
                from: new_states.integerise(from),
                to: new_states.integerise(to),
                label,
                weight
            }
        ).collect();



        let mut intersect_finals = Vec::new();
        for qf in finals {
            for qf_ in ffinals.iter() {
                if let Some(i) = new_states.find_key(&(*qf, *qf_)) {
                    intersect_finals.push(i);
                }
            }
        }

        FiniteAutomaton::from_integerized(
            intersect_arcs,
            new_states.integerise((*initial, finitial)),
            intersect_finals,
            Rc::clone(labels)
        )
    }

    fn generate<'a>(self, beam: usize) -> Box<Iterator<Item = Vec<T>> + 'a> where T: 'a {
        let FiniteAutomaton{
            initial,
            finals,
            arcs,
            labels
        } = self;

        let heuristics = {
            let mut backwards_transitions = HashMap::new();
            for (from, arcs_from) in &arcs {
                for (_, &(to, weight)) in arcs_from {
                    let bw_to = backwards_transitions.entry(to).or_insert(BinaryHeap::new());
                    bw_to.push((weight, *from));
                }
            }
            heuristics(backwards_transitions, finals.as_slice())
        };

        Box::new(
            UniqueSearch::weighted(
                vec![(initial, vec![], LogDomain::one())],
                Box::new(
                    move | &(ref q, ref word, weight) | {
                        let mut successors = Vec::new();
                        for (label, &(to, w)) in arcs.get(q).unwrap_or(&HashMap::new()) {
                            let mut word_ = word.clone();
                            word_.push(label.clone());
                            successors.push((to, word_, weight * w))
                        }
                        successors
                    }
                ),
                Box::new(
                    move | &(ref q, _, w) | {
                        (*heuristics.get(q).unwrap_or(&LogDomain::zero()) * w).pow(-1.0)
                    }
                ),
                Capacity::Limit(beam)

            ).filter(move | &(ref q, _, _) | finals.contains(q))
             .map(
                move | (_, wi, _) | 
                    wi.into_iter()
                      .map(| i | labels.find_value(i).unwrap())
                      .cloned()
                      .collect()
            )
        )
    }
}

use super::GeneratorAutomaton;

impl<T> GeneratorAutomaton<T> for FiniteAutomaton<T>
where
    T: Eq + Hash + Clone,
{
    fn get_integeriser(&self) -> Rc<HashIntegeriser<T>> {
        Rc::clone(&self.labels)
    }

    fn generate<'a>(&self, filter: FiniteAutomaton<T>, beam: usize) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        let intersection = self.intersect(filter);
        intersection.generate(beam)
    }
}

use std::borrow::Borrow;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
type SerializerRepresentation<T> = (
    usize,
    Vec<usize>,
    HashMap<usize, HashMap<usize, (usize, LogDomain<f32>)>>,
    HashIntegeriser<T>,
);

impl<T> Serialize for FiniteAutomaton<T>
where
    T: Serialize + Eq + Hash,
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

        (initial, finals, arcs, Borrow::<HashIntegeriser<T>>::borrow(labels)).serialize(s)
    }
}

impl<'de, T> Deserialize<'de> for FiniteAutomaton<T>
where
    T: Deserialize<'de> + Eq + Hash + Clone,
{
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let (initial, finals, arcs, labels) = SerializerRepresentation::deserialize(d)?;

        Ok(
            FiniteAutomaton{
                initial,
                finals,
                arcs,
                labels: Rc::new(labels)
            }
        )
    }
}

// #[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
// struct Item<T>(LogDomain<f32>, usize, Vec<T>);
// #[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
// struct Trans<T>(LogDomain<f32>, usize, T);

// impl<T> FiniteAutomaton<T>
// where
//     T: Hash + Ord + Clone + 'static + Debug,
// {
//     pub fn generate(self, beam: usize) -> Box<Iterator<Item = Vec<T>>> {
//         let FiniteAutomaton{ initial, finals, arcs } = self;

//         let heuristics = ::cs_representation::dka::heuristics(
//             arcs.iter()
//                 .map(| (from, arcs) |
//                      (*from, arcs.iter().map(| (_, &(to, w)) | (w, to)).collect())
//                  )
//                 .collect(),
//             finals.as_slice()
//         );

//         let mut initagenda = BoundedPriorityQueue::new(
//             beam,
//             Box::new(move |&Item(ref w, ref q, _)| {
//                 (*heuristics.get(q).unwrap_or(&LogDomain::zero()) * *w).pow(-1.0)
//             }),
//         );
//         initagenda.enqueue(Item(LogDomain::one(), initial, vec![]));

//         Box::new(
//             DeterministicSearch {
//                 agenda: initagenda,
//                 item_to_key: Box::new(|&Item(_, ref from, _)| from),
//                 key_to_rules: arcs,
//                 apply: Box::new(
//                     |&Item(ref wi, _, ref word), &Arc{ ref weight, ref label, ref to, .. }| {
//                         let mut w: Vec<T> = word.clone();
//                         w.push(label.clone());
//                         Item(*wi * *weight, *to, w)
//                     },
//                 ),
//                 accepting: Box::new(move |&Item(_, ref q, _)| finals.contains(q)),
//             }.map(|Item(_, _, w)| w),
//         )
//     }
// }
