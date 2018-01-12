use std::collections::{BinaryHeap, HashMap, BTreeSet};
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
pub fn heuristics<Q>(rules: HashMap<Q, BinaryHeap<(LogDomain<f64>, Q)>>, qfs: &[Q]) -> HashMap<Q, LogDomain<f64>>
where
    Q: Ord + Clone + Hash,
{
    #[derive(Clone, Debug)]
    struct SearchItem<Q>(Q, LogDomain<f64>);
    impl<Q> PartialEq for SearchItem<Q>
    where
        Q: PartialEq,
    {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }
    impl<Q> Eq for SearchItem<Q>
    where
        Q: Eq,
    {
    }
    impl<Q> PartialOrd for SearchItem<Q>
    where
        Q: PartialOrd,
    {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            self.0.partial_cmp(&other.0)
        }
    }
    impl<Q> Ord for SearchItem<Q>
    where
        Q: Ord,
    {
        fn cmp(&self, other: &Self) -> Ordering {
            self.0.cmp(&other.0)
        }
    }

    Search::weighted(
        qfs.iter().map(|q| SearchItem(q.clone(), LogDomain::one())),
        move |&SearchItem(ref to, ref w)| {
            if let Some(arcs_to) = rules.get(to) {
                arcs_to
                    .iter()
                    .map(|&(ref w_, ref from)| SearchItem(from.clone(), *w_ * *w))
                    .collect()
            } else {
                Vec::new()
            }
        },
        Box::new(|&SearchItem(_, ref w)| (*w).pow(-1.0)),
    ).uniques()
     .map(|SearchItem(q, w)| (q, w))
     .collect()
}

impl<T, W> KellerAutomaton<T, W>
where
    T: Hash + Eq + Clone,
    W: Copy + Ord
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
    pub fn intersect<W>(self, dfa: FiniteAutomaton<T, W>) -> Self {
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
            },
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
        let heuristics = self.heuristics();
        eprintln!("{:?}", heuristics);

        let KellerAutomaton {
            arcs,
            initial,
            finals,
            labels,
        } = self;


        Box::new(
            {
                let it = Search::weighted(
                    vec![(LogDomain::one(), initial, vec![], vec![])],
                    move |&(weight_, q, ref word, ref keller)| {
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
                    },
                    Box::new(
                        move |&(weight, ref q, ref w, ref pd)| {
                            let h = pd.iter()
                                .map(|pds| heuristics.get(q).and_then(|&(ref m, _)| m.get(pds).map(|w| *w)).unwrap_or(LogDomain::zero()))
                                .min()
                                .unwrap_or(heuristics.get(q).map(|&(_, w)| w).unwrap_or(LogDomain::zero()));
                            let hs: Vec<LogDomain<f64>> = pd.iter().map(|pds| heuristics.get(q).and_then(|&(ref m, _)| m.get(pds).map(|w| *w)).unwrap_or(LogDomain::zero())).collect();
                            eprintln!("{}, {:?}, {:?} → {:?}", q, w, pd, hs);
                            (h * weight).pow(-1.0)
                        }
                    )
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
    
    fn heuristics(&self) -> IntMap<(IntMap<LogDomain<f64>>, LogDomain<f64>)> {
        #[derive(Clone, Debug)]
        struct SearchItem(usize, BTreeSet<usize>, LogDomain<f64>);
        impl PartialEq for SearchItem {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0 && self.1 == other.1
            }
        }
        impl Eq for SearchItem{}
        impl PartialOrd for SearchItem {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                match self.0.partial_cmp(&other.0) {
                    Some(Ordering::Equal) => self.1.partial_cmp(&other.1),
                    o => o
                }
            }
        }
        impl Ord for SearchItem {
            fn cmp(&self, other: &Self) -> Ordering {
                match self.0.cmp(&other.0) {
                    Ordering::Equal => self.1.cmp(&other.1),
                    o => o
                }
            }
        }

        let starts = self.finals.iter().map(|q| SearchItem(*q, BTreeSet::new(), LogDomain::one()));
        
        let mut bwtransitions: Vec<IntMap<(LogDomain<f64>, KellerOp<usize>)>> = Vec::new();
        for (from, farcs) in self.arcs.iter().enumerate() {
            for (_, &(to, weight, ref op)) in farcs {
                vec_entry(&mut bwtransitions, to).insert(from, (weight, op.clone()));
            }
        }
        
        let mut heuristics = IntMap::default();
        let mut removeables_per_state = IntMap::default();
        for SearchItem(q, removeables, w) in Search::weighted(
            starts, 
            |&SearchItem(q, ref removeables, w)| {
                let olds = removeables_per_state.entry(q).or_insert(BTreeSet::new());
                let news: BTreeSet<usize> = removeables.difference(olds).cloned().collect();
                olds.extend(news.iter().cloned());

                let mut succ = Vec::new();
                for (from, &(weight, ref op)) in bwtransitions.get(q).unwrap_or(&IntMap::default()) {
                    let mut removeables_ = news.clone();
                    match *op {
                        KellerOp::Remove(i) | KellerOp::Replace(i, _) => {
                            removeables_.insert(i);
                        },
                        _ => ()
                    }
                    succ.push(SearchItem(*from, removeables_, w * weight));
                }
                succ
            },
            Box::new(|&SearchItem(_, _, w)| w.pow(-1.0))
        ).uniques() {
            let map = heuristics.entry(q).or_insert((IntMap::default(), w));
            for rem in removeables {
                map.0.entry(rem).or_insert(w);
            }
        }
        heuristics
    }
}

use super::GeneratorAutomaton;
impl<T> GeneratorAutomaton<T> for KellerAutomaton<T, LogDomain<f64>>
where
    T: Eq + Clone + Hash,
{

    fn size(&self) -> usize {
        self.arcs.iter().flat_map(|map| map.values()).count()
    }

    fn get_integeriser(&self) -> Rc<HashIntegeriser<T>> {
        Rc::clone(&self.labels)
    }
    
    fn intersect(&self, other: FiniteAutomaton<T, ()>) -> Self {
        self.clone().intersect(other)
    }

    fn generate<'a>(
        self,
        beam: Capacity,
    ) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        self.generate(beam)
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pda() {
        let arcs = vec![
            KellerArc{ from: 0, to: 0, label: "A", op: KellerOp::Add(0), weight: LogDomain::new(1.0).unwrap()},
            KellerArc{ from: 0, to: 1, label: "B", op: KellerOp::Remove(0), weight: LogDomain::new(0.5).unwrap()},
            KellerArc{ from: 1, to: 1, label: "B", op: KellerOp::Remove(0), weight: LogDomain::new(1.0).unwrap()},
            KellerArc{ from: 0, to: 0, label: "C", op: KellerOp::Add(1), weight: LogDomain::new(1.0).unwrap()}
        ];
        
        eprintln!("{:?}", KellerAutomaton::new(arcs, 0, vec![1]).heuristics());
    }

}
