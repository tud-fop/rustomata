use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;

use log_domain::LogDomain;
use num_traits::{One, Zero};
use integeriser::{HashIntegeriser, Integeriser};

use recognisable::{Search, UniqueSearch};
use super::{FiniteAutomaton, FiniteArc};
use std::cmp::Ordering;
use std::rc::Rc;
use util::agenda::Capacity;

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Serialize, Deserialize)]
pub enum KellerOp<S> {
    Nothing,
    Remove(S),
    Add(S),
    Replace(S, S),
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Serialize, Deserialize)]
pub struct KellerArc<T, Q, S> {
    pub from: Q,
    pub to: Q,
    pub label: T,
    pub op: KellerOp<S>,
    pub weight: LogDomain<f32>,
}

#[derive(Debug, Clone)]
pub struct KellerAutomaton<T>
where
    T: Hash + Eq + Clone,
{
    pub initial: usize,
    pub finals: Vec<usize>,
    pub arcs: HashMap<usize, HashMap<usize, (usize, LogDomain<f32>, KellerOp<usize>)>>,
    pub labels: Rc<HashIntegeriser<T>>,
}

impl<S> KellerOp<S>
where
    S: Clone + PartialEq,
{
    pub fn apply(&self, mut keller: Vec<S>) -> Option<Vec<S>> {
        match self {
            &KellerOp::Nothing => Some(keller),
            &KellerOp::Add(ref s) => {
                keller.push(s.clone());
                Some(keller)
            }
            &KellerOp::Remove(ref s) => if Some(s) == keller.pop().as_ref() {
                Some(keller)
            } else {
                None
            },
            &KellerOp::Replace(ref s, ref s_) => if Some(s) == keller.pop().as_ref() {
                keller.push(s_.clone());
                Some(keller)
            } else {
                None
            },
        }
    }

    fn apply_with_capacity(&self, keller: &Vec<S>, cap: usize) -> Option<Vec<S>> {
        let mut succ = match self {
            &KellerOp::Nothing => Some(keller.clone()),
            &KellerOp::Add(ref s) => {
                let mut keller_ = keller.clone();
                keller_.insert(0, s.clone());
                Some(keller_)
            }
            &KellerOp::Remove(ref s) => {
                let mut keller_ = keller.clone();
                if keller_.is_empty() || &keller_.remove(0) == s {
                    Some(keller_)
                } else {
                    None
                }
            },
            &KellerOp::Replace(ref s, ref s_) => {
                let mut keller_ = keller.clone();
                if keller_.is_empty() || &keller_.remove(0) == s {
                    keller_.insert(0, s_.clone());
                    Some(keller_)
                } else {
                    None
                }
            }
        };
        if let Some(keller_) = succ.as_mut() {
            keller_.truncate(cap);
        }
        succ
    } 
}

pub fn heuristics<Q, W>(rules: HashMap<Q, BinaryHeap<(W, Q)>>, qfs: &[Q]) -> HashMap<Q, W>
where
    Q: Ord + Clone + Hash,
    W: Ord + Copy + One,
{
    #[derive(Clone)]
    struct SearchItem<Q, W>(Q, W);
    impl<Q, W> PartialEq for SearchItem<Q, W>
    where
        Q: PartialEq,
    {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }
    impl<Q, W> Eq for SearchItem<Q, W>
    where
        Q: Eq,
    {
    }
    impl<Q, W> PartialOrd for SearchItem<Q, W>
    where
        Q: PartialOrd,
    {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            self.0.partial_cmp(&other.0)
        }
    }
    impl<Q, W> Ord for SearchItem<Q, W>
    where
        Q: Ord,
    {
        fn cmp(&self, other: &Self) -> Ordering {
            self.0.cmp(&other.0)
        }
    }

    Search::weighted(
        qfs.iter().map(|q| SearchItem(q.clone(), W::one())),
        Box::new(move |&SearchItem(ref to, ref w)| {
            if let Some(arcs_to) = rules.get(to) {
                arcs_to
                    .iter()
                    .map(|&(ref w_, ref from)| SearchItem(from.clone(), *w_ * *w))
                    .collect()
            } else {
                Vec::new()
            }
        }),
        Box::new(|&SearchItem(_, ref w)| *w),
    ).map(|SearchItem(q, w)| (q, w))
        .collect()
}

impl<T> KellerAutomaton<T>
where
    T: Hash + Eq + Clone,
{
    pub fn new<Q, S>(oarcs: Vec<KellerArc<T, Q, S>>, oinitial: Q, ofinals: Vec<Q>) -> Self
    where
        Q: Hash + Eq + Clone,
        S: Hash + Eq + Clone,
    {
        let mut labels = HashIntegeriser::new();
        let mut keller_symbols = HashIntegeriser::new();
        let mut states = HashIntegeriser::new();

        let initial = states.integerise(oinitial);
        let finals = ofinals.into_iter().map(|q| states.integerise(q)).collect();

        let mut arcs = HashMap::new();
        for KellerArc {
            from,
            to,
            label,
            op,
            weight,
        } in oarcs
        {
            let iop = match op {
                KellerOp::Nothing => KellerOp::Nothing,
                KellerOp::Add(s) => KellerOp::Add(keller_symbols.integerise(s)),
                KellerOp::Remove(s) => KellerOp::Remove(keller_symbols.integerise(s)),
                KellerOp::Replace(s, s_) => {
                    KellerOp::Replace(keller_symbols.integerise(s), keller_symbols.integerise(s_))
                }
            };
            let arcs_from = arcs.entry(states.integerise(from))
                .or_insert(HashMap::new());
            arcs_from.insert(
                labels.integerise(label),
                (states.integerise(to), weight, iop),
            );
        }

        KellerAutomaton {
            initial,
            finals,
            arcs,
            labels: Rc::new(labels),
        }
    }

    pub fn approximate(self, depth: usize) -> FiniteAutomaton<T> {
        let KellerAutomaton{ initial, finals, arcs, labels } = self;
        let mut new_states = HashIntegeriser::new();

        let mut agenda: Vec<FiniteArc<(usize, Vec<usize>), usize>> = Vec::new();
        for (label, &(to, weight, ref op)) in arcs.get(&initial).unwrap_or(&HashMap::new()) {
            let keller = Vec::new();
            if let Some(keller_) = op.apply_with_capacity(&keller, depth){
                agenda.push(
                    FiniteArc{
                        label: *label,
                        from: (initial, keller),
                        to: (to, keller_),
                        weight
                    }
                )
            }
        }

        let transitions = Search::unweighted(
            agenda,
            Box::new(
                | &FiniteArc{ to: (from, ref keller), .. } | {
                    let mut succ = Vec::new();
                    for (label, &(to, weight, ref op)) in arcs.get(&from).unwrap_or(&HashMap::new()) {
                        if let Some(keller_) = op.apply_with_capacity(keller, depth) {
                            succ.push(
                                FiniteArc{
                                    label: *label,
                                    weight,
                                    from: (from, keller.clone()),
                                    to: (to, keller_)
                                }
                            );
                        }
                    }
                    succ
                }
            )
        ).map(
            | FiniteArc{ from, to, label, weight } |
            FiniteArc{
                from: new_states.integerise(from),
                to: new_states.integerise(to),
                label,
                weight
            }
        ).collect();
        
        let new_finals: Vec<usize> = finals.into_iter().filter_map(| q | new_states.find_key(&(q, Vec::new()))).collect();

        FiniteAutomaton::from_integerized(
            transitions,
            new_states.find_key(&(initial, Vec::new())).unwrap(),
            new_finals,
            labels
        )
    }
}

impl<T> KellerAutomaton<T>
where
    T: Hash + Eq + Clone,
{
    pub fn intersect(self, dfa: FiniteAutomaton<T>) -> KellerAutomaton<T> {
        let KellerAutomaton {
            initial,
            finals,
            arcs,
            labels,
        } = self;
        let FiniteAutomaton {
            initial: f_initial,
            finals: f_finals,
            arcs: f_arcs,
            ..
        } = dfa;

        let mut agenda: Vec<KellerArc<usize, (usize, usize), usize>> = Vec::new();
        for (label, &(to, weight, ref op)) in arcs.get(&initial).unwrap_or(&HashMap::new()) {
            if let Some(&(fto, _)) = f_arcs.get(&f_initial).and_then(|m| m.get(label)) {
                agenda.push(KellerArc {
                    from: (initial, f_initial),
                    to: (to, fto),
                    label: *label,
                    weight: weight,
                    op: op.clone(),
                });
            }
        }

        let mut new_arcs = HashMap::new();
        let mut states = HashIntegeriser::new();
        for KellerArc {
            from,
            to,
            label,
            weight,
            op,
        } in Search::unweighted(
            agenda,
            Box::new(move |arc| {
                let mut heap = Vec::new();
                let &KellerArc { to: (kq, fq), .. } = arc;
                for (label, &(to, weight, ref op)) in arcs.get(&kq).unwrap_or(&HashMap::new()) {
                    if let Some(&(fto, _)) = f_arcs.get(&fq).and_then(|m| m.get(label)) {
                        heap.push(KellerArc {
                            from: (kq, fq),
                            to: (to, fto),
                            label: *label,
                            op: op.clone(),
                            weight,
                        });
                    }
                }
                heap
            }),
        ) {
            let arcs_from = new_arcs
                .entry(states.integerise(from))
                .or_insert(HashMap::new());
            arcs_from.insert(label, (states.integerise(to), weight, op));
        }

        let mut new_finals: Vec<usize> = Vec::new();
        for kq in finals {
            for fq in f_finals.iter() {
                if let Some(q) = states.find_key(&(kq, *fq)) {
                    new_finals.push(q);
                }
            }
        }

        KellerAutomaton {
            arcs: new_arcs,
            initial: states.find_key(&(initial, f_initial)).unwrap(),
            finals: new_finals,
            labels,
        }
    }

    pub fn generate<'a>(self, beam: usize) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        let KellerAutomaton {
            arcs,
            initial,
            finals,
            labels,
        } = self;

        let heuristics: HashMap<usize, LogDomain<f32>> = {
            let mut backwards_transitions = HashMap::new();
            for (from, arcs_from) in &arcs {
                for (_, &(to, weight, _)) in arcs_from {
                    let bw_to = backwards_transitions.entry(to).or_insert(BinaryHeap::new());
                    bw_to.push((weight, *from));
                }
            }
            heuristics(backwards_transitions, finals.as_slice())
        };

        Box::new(
            UniqueSearch::weighted(
                vec![(LogDomain::one(), initial.clone(), vec![], vec![])],
                Box::new(move |&(weight_, ref q, ref word, ref keller)| {
                    let mut results = Vec::new();
                    if let Some(arcs_from) = arcs.get(q) {
                        for (label, &(to, weight, ref op)) in arcs_from {
                            if let Some(keller_) = op.apply(keller.clone()) {
                                let mut word_ = word.clone();
                                word_.push(*label);
                                results.push((weight * weight_, to, word_, keller_));
                            }
                        }
                    }
                    results
                }),
                Box::new(move |&(weight, ref q, _, _)| {
                    (*heuristics.get(q).unwrap_or(&LogDomain::zero()) * weight).pow(-1.0)
                }),
                Capacity::Limit(beam)
            ).filter(move |&(_, ref q, _, ref keller)| {
                finals.contains(q) && keller.is_empty()
            })
                .map(move |(_, _, word, _)| {
                    word.into_iter()
                        .map(|i| labels.find_value(i).unwrap())
                        .cloned()
                        .collect()
                }),
        )
    }
}

use super::GeneratorAutomaton;
impl<T> GeneratorAutomaton<T> for KellerAutomaton<T>
where
    T: Eq + Clone + Hash,
{
    fn get_integeriser(&self) -> Rc<HashIntegeriser<T>> {
        Rc::clone(&self.labels)
    }

    fn generate<'a>(&self, fsa: FiniteAutomaton<T>, beam: usize) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        self.clone().intersect(fsa).generate(beam)
    }
}

use std::borrow::Borrow;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

type SerializedRepresentation<T> = (
    usize,
    Vec<usize>,
    HashMap<usize, HashMap<usize, (usize, LogDomain<f32>, KellerOp<usize>)>>,
    HashIntegeriser<T>,
);

impl<T> Serialize for KellerAutomaton<T>
where
    T: Serialize + Hash + Eq + Clone,
{
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let &KellerAutomaton {
            ref initial,
            ref finals,
            ref arcs,
            ref labels,
        } = self;
        (initial, finals, arcs, Borrow::<HashIntegeriser<T>>::borrow(labels)).serialize(s)
    }
}

impl<'de, T> Deserialize<'de> for KellerAutomaton<T>
where
    T: Deserialize<'de> + Hash + Eq + Clone,
{
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let (initial, finals, arcs, labels) = SerializedRepresentation::deserialize(d)?;
        Ok(KellerAutomaton {
            initial,
            finals,
            arcs,
            labels: Rc::new(labels),
        })
    }
}
