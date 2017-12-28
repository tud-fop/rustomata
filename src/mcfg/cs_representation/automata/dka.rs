use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;

use log_domain::LogDomain;
use num_traits::{One, Zero};
use integeriser::{HashIntegeriser, Integeriser};

use recognisable::{Search};
use super::{FiniteArc, FiniteAutomaton};
use std::cmp::Ordering;
use std::rc::Rc;
use util::agenda::Capacity;
use util::{vec_entry, IntMap};

/// An operation on a Keller storage.
/// The set of ops is limited to removal, addition and replacement.
#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Serialize, Deserialize, Copy)]
pub enum KellerOp<S> {
    Nothing,
    Remove(S),
    Add(S),
    Replace(S, S),
}

/// A weighted transition of a `KellerAutomaton`.
#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Serialize, Deserialize)]
pub struct KellerArc<T, Q, S, W> {
    pub from: Q,
    pub to: Q,
    pub label: T,
    pub op: KellerOp<S>,
    pub weight: W,
}

/// A deterministic `KellerAutomaton`.
#[derive(Debug, Clone)]
pub struct KellerAutomaton<T, W>
where
    T: Hash + Eq + Clone,
{
    pub initial: usize,
    pub finals: Vec<usize>,
    pub arcs: Vec<IntMap<(usize, W, KellerOp<usize>)>>,
    pub labels: Rc<HashIntegeriser<T>>,
}

impl<S> KellerOp<S>
where
    S: Copy + PartialEq,
{
    /// Applies a `KellerOp` to a Keller storage
    /// and returns a Keller if it succeeds.
    /// The `Clone` is only performed if the operation
    /// is applicable.
    fn apply(&self, keller: &Vec<S>) -> Option<Vec<S>> {
        match *self {
            KellerOp::Nothing => Some(keller.clone()),
            KellerOp::Add(s) => {
                let mut keller_ = keller.clone();
                keller_.push(s);
                Some(keller_)
            }
            KellerOp::Remove(s) => keller
                .split_last()
                .and_then(|(&s_, f)| if s_ == s { Some(f.to_vec()) } else { None }),
            KellerOp::Replace(s, s_) => 
            keller.split_last().and_then(
                | (&vs, f) | {
                    if vs == s {
                        let mut keller_ = f.to_vec();
                        keller_.push(s_);
                        Some(keller_)                        
                    } else {
                        None
                    }
                }
            )
        }
    }

    /// Same as `apply`, but:
    /// * each Keller storage is limited to a capacity
    /// * if the capacity is reached, the application will cut off
    ///   elements at the bottom of the Keller storage
    /// * removal and replacement operations are still applicable
    ///   if the Keller is empty, due to compensation of cut offs
    fn apply_with_capacity(&self, keller: &Vec<S>, cap: usize) -> Option<Vec<S>> {
        let mut succ = match *self {
            KellerOp::Nothing => Some(keller.clone()),
            KellerOp::Add(ref s) => {
                let mut keller_ = keller.clone();
                keller_.insert(0, s.clone());
                Some(keller_)
            }
            KellerOp::Remove(ref s) => {
                let mut keller_ = keller.clone();
                if keller_.is_empty() || &keller_.remove(0) == s {
                    Some(keller_)
                } else {
                    None
                }
            }
            KellerOp::Replace(ref s, ref s_) => {
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

/// Computes the least weight of each node in a graph to
/// any target node using `Mul`tiplicative weight monoid.
pub fn heuristics<Q, W>(rules: HashMap<Q, BinaryHeap<(W, Q)>>, qfs: &[Q]) -> HashMap<Q, W>
where
    Q: Ord + Clone + Hash + ::std::fmt::Debug,
    W: Ord + Copy + One + ::std::fmt::Debug,
{
    #[derive(Clone, Debug)]
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
    ).uniques()
     .map(|SearchItem(q, w)| (q, w))
     .collect()
}

impl<T, W> KellerAutomaton<T, W>
where
    T: Hash + Eq + Clone + ::std::fmt::Debug,
    W: Copy + Ord + ::std::fmt::Debug
{
    /// Creates a deterministic `KellerAutomaton` using a sequence of transisitons.
    /// If there are multiple transitions from the same state with the same label,
    /// the last one will be used.
    pub fn new<Q, S>(oarcs: Vec<KellerArc<T, Q, S, W>>, oinitial: Q, ofinals: Vec<Q>) -> Self
    where
        Q: Hash + Eq + Clone,
        S: Hash + Eq + Clone,
    {
        let mut labels = HashIntegeriser::new();
        let mut keller_symbols = HashIntegeriser::new();
        let mut states = HashIntegeriser::new();

        let initial = states.integerise(oinitial);

        let mut arcs = Vec::new();
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
            vec_entry::<IntMap<(usize, W, KellerOp<usize>)>>(
                &mut arcs,
                states.integerise(from),
            ).insert(
                labels.integerise(label),
                (states.integerise(to), weight, iop),
            );
        }

        let finals = ofinals
            .into_iter()
            .filter_map(|q| states.find_key(&q))
            .collect();

        KellerAutomaton {
            initial,
            finals,
            arcs,
            labels: Rc::new(labels),
        }
    }

    /// Computes a deterministic `FiniteAutomaton` that accepts a superset
    /// of the `KellerAutomaton`s language.
    /// The approximation limits each Keller storage of a run to a given depth.
    /// The set of all resulting runs is used to read off a set of `FiniteArcs`.
    pub fn approximate(self, depth: usize) -> FiniteAutomaton<T, W> {
        let KellerAutomaton {
            initial,
            finals,
            arcs,
            labels,
        } = self;
        let mut new_states = HashIntegeriser::new();

        let mut agenda: Vec<FiniteArc<(usize, Vec<usize>), usize, W>> = Vec::new();
        for (label, &(to, weight, ref op)) in arcs.get(initial).unwrap_or(&IntMap::default()) {
            let keller = Vec::new();
            if let Some(keller_) = op.apply_with_capacity(&keller, depth) {
                agenda.push(FiniteArc {
                    label: *label,
                    from: (initial, keller),
                    to: (to, keller_),
                    weight,
                })
            }
        }

        let transitions = Search::unweighted(
            agenda,
            Box::new(
                |&FiniteArc {
                     to: (from, ref keller),
                     ..
                 }| {
                    let mut succ = Vec::new();
                    for (label, &(to, weight, ref op)) in
                        arcs.get(from).unwrap_or(&IntMap::default())
                    {
                        if let Some(keller_) = op.apply_with_capacity(keller, depth) {
                            succ.push(FiniteArc {
                                label: *label,
                                weight,
                                from: (from, keller.clone()),
                                to: (to, keller_),
                            });
                        }
                    }
                    succ
                },
            ),
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

        let new_finals: Vec<usize> = finals
            .into_iter()
            .filter_map(|q| new_states.find_key(&(q, Vec::new())))
            .collect();

        FiniteAutomaton::from_integerized(
            transitions,
            new_states.find_key(&(initial, Vec::new())).unwrap(),
            new_finals,
            labels,
        )
    }
}

impl<T> KellerAutomaton<T, LogDomain<f64>>
where
    T: Hash + Eq + Clone,
{
    /// Computes the Hadamard product of a deterministic `KellerAutomaton` and
    /// a deterministic `FiniteAutomaton`.
    pub fn intersect<W: ::std::fmt::Debug>(self, dfa: FiniteAutomaton<T, W>) -> Self {
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

        let mut agenda: Vec<KellerArc<usize, (usize, usize), usize, LogDomain<f64>>> = Vec::new();
        for (label, &(to, weight, ref op)) in arcs.get(initial).unwrap_or(&IntMap::default()) {
            if let Some(&(fto, _)) = f_arcs.get(f_initial).and_then(|m| m.get(label)) {
                agenda.push(KellerArc {
                    from: (initial, f_initial),
                    to: (to, fto),
                    label: *label,
                    weight: weight,
                    op: op.clone(),
                });
            }
        }

        let mut new_arcs = Vec::new();
        let mut states = HashIntegeriser::new();
        for KellerArc {
            from,
            to,
            label,
            weight,
            op,
        } in Search::unweighted(
            agenda,
            Box::new(
                move |arc| {
                    let mut heap = Vec::new();
                    let &KellerArc { to: (kq, fq), .. } = arc;
                    for (label, &(to, weight, ref op)) in arcs.get(kq).unwrap_or(&IntMap::default()) {
                        if let Some(&(fto, _)) = f_arcs.get(fq).and_then(|m| m.get(label)) {
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
                }
            ),
        ).uniques() {
            vec_entry::<IntMap<(usize, LogDomain<f64>, KellerOp<usize>)>>(
                &mut new_arcs,
                states.integerise(from),
            ).insert(label, (states.integerise(to), weight, op));
        }

        let mut new_finals: Vec<usize> = Vec::new();
        for kq in finals {
            for fq in &f_finals {
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

    /// Creates an `Iterator` over all words accepted by a `KellerAutomaton`.
    pub fn generate<'a>(self, beam: Capacity) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        let KellerAutomaton {
            arcs,
            initial,
            finals,
            labels,
        } = self;

        let heuristics: HashMap<usize, LogDomain<f64>> = {
            let mut backwards_transitions = HashMap::new();
            for (from, arcs_from) in arcs.iter().enumerate() {
                for &(to, weight, _) in arcs_from.values() {
                    backwards_transitions
                        .entry(to)
                        .or_insert_with(BinaryHeap::new)
                        .push((weight, from));
                }
            }
            heuristics(backwards_transitions, finals.as_slice())
        };

        Box::new(
            {
                let it = Search::weighted(
                    vec![(LogDomain::one(), initial, vec![], vec![])],
                    Box::new(move |&(weight_, q, ref word, ref keller)| {
                        let mut results = Vec::new();
                        if let Some(arcs_from) = arcs.get(q) {
                            for (label, &(to, weight, ref op)) in arcs_from {
                                if let Some(keller_) = op.apply(keller) {
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
                    })
                );
                if let Capacity::Limit(i) = beam {
                    it.beam(i)
                } else { it }
            }.filter(
                move |&(_, ref q, _, ref keller)| {
                    finals.contains(q) && keller.is_empty()
                }
            ).map(
                move |(_, _, word, _)| {
                    word.into_iter()
                        .map(|i| labels.find_value(i).unwrap())
                        .cloned()
                        .collect()
                }
            ),
        )
    }
}

use super::GeneratorAutomaton;
impl<T> GeneratorAutomaton<T> for KellerAutomaton<T, LogDomain<f64>>
where
    T: Eq + Clone + Hash,
{
    fn get_integeriser(&self) -> Rc<HashIntegeriser<T>> {
        Rc::clone(&self.labels)
    }

    fn generate<'a>(
        &self,
        fsa: FiniteAutomaton<T, ()>,
        beam: Capacity,
    ) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        self.clone().intersect(fsa).generate(beam)
    }
}

use std::borrow::Borrow;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// This tuple type is used to exploit the implementation of
/// `Serialize` and `Deserialize` for tuples.
/// It holds the conents of `KellerAutomaton`.
type SerializedRepresentation<T, W> = (
    usize,
    Vec<usize>,
    Vec<IntMap<(usize, W, KellerOp<usize>)>>,
    HashIntegeriser<T>,
);

impl<T, W> Serialize for KellerAutomaton<T, W>
where
    T: Serialize + Hash + Eq + Clone,
    W: Serialize
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
        (
            initial,
            finals,
            arcs,
            Borrow::<HashIntegeriser<T>>::borrow(labels),
        ).serialize(s)
    }
}

impl<'de, T, W> Deserialize<'de> for KellerAutomaton<T, W>
where
    T: Deserialize<'de> + Hash + Eq + Clone,
    W: Deserialize<'de>
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

use std::fmt::{Display, Error, Formatter};

impl<T, W> Display for KellerAutomaton<T, W>
where
    T: Display + Hash + Eq + Clone,
    W: Display
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let &KellerAutomaton {
            ref arcs,
            ref initial,
            ref finals,
            ref labels,
        } = self;

        let mut buffer = String::new();
        for (from, arcs_from) in arcs.iter().enumerate() {
            for (label, &(ref to, ref weight, ref op)) in arcs_from {
                buffer.push_str(&format!(
                    "{} → [{}] {} / {:?} # {}\n",
                    from,
                    labels.find_value(*label).unwrap(),
                    to,
                    op,
                    weight
                ));
            }
        }
        write!(f, "initial: {}, finals: {:?}\n{}", initial, finals, &buffer)
    }
}
