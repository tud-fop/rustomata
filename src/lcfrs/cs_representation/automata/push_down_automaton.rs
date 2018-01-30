use std::collections::BTreeSet;
use std::hash::Hash;

use log_domain::LogDomain;
use num_traits::{One, Zero};
use integeriser::{HashIntegeriser, Integeriser};

use recognisable::{Search, WeightedSearchItem};
use super::{FiniteArc, FiniteAutomaton};
use std::rc::Rc;
use util::agenda::Capacity;
use util::{vec_entry, IntMap};

/// An operation on a push-down.
/// The set of ops is limited to removal, addition and replacement.
#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Serialize, Deserialize, Copy)]
pub enum Operation<S> {
    Nothing,
    Remove(S),
    Add(S),
    Replace(S, S),
}

/// A weighted transition of a push-down `PushDownAutomaton`.
#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Serialize, Deserialize)]
pub struct PushDownArc<T, Q, S, W> {
    pub from: Q,
    pub to: Q,
    pub label: T,
    pub op: Operation<S>,
    pub weight: W,
}

/// A deterministic push-down `PushDownAutomaton`.
#[derive(Debug, Clone)]
pub struct PushDownAutomaton<T, W>
where
    T: Hash + Eq + Clone,
{
    pub initial: usize,
    pub finals: Vec<usize>,
    pub arcs: Vec<IntMap<(usize, W, Operation<usize>)>>,
    pub labels: Rc<HashIntegeriser<T>>,
}

impl<S> Operation<S>
where
    S: Copy + PartialEq,
{
    /// Applies a `Operation` to a push-down
    /// and returns a push-down if it succeeds.
    /// The `Clone` is only performed if the operation
    /// is applicable.
    fn apply(&self, pd: &Vec<S>) -> Option<Vec<S>> {
        match *self {
            Operation::Nothing => Some(pd.clone()),
            Operation::Add(s) => {
                let mut pd_ = pd.clone();
                pd_.push(s);
                Some(pd_)
            }
            Operation::Remove(s) => pd.split_last()
                .and_then(|(&s_, f)| if s_ == s { Some(f.to_vec()) } else { None }),
            Operation::Replace(s, s_) => pd.split_last().and_then(|(&vs, f)| {
                if vs == s {
                    let mut pd_ = f.to_vec();
                    pd_.push(s_);
                    Some(pd_)
                } else {
                    None
                }
            }),
        }
    }

    /// Same as `apply`, but:
    /// * each push-down is limited to a capacity
    /// * if the capacity is reached, the application will cut off
    ///   elements at the bottom of the push-down
    /// * removal and replacement operations are still applicable
    ///   if the push-down is empty, due to compensation of cut offs
    fn apply_with_capacity(&self, pd: &Vec<S>, cap: usize) -> Option<Vec<S>> {
        let mut succ = match *self {
            Operation::Nothing => Some(pd.clone()),
            Operation::Add(ref s) => {
                let mut pd_ = pd.clone();
                pd_.insert(0, s.clone());
                Some(pd_)
            }
            Operation::Remove(ref s) => {
                let mut pd_ = pd.clone();
                if pd_.is_empty() || &pd_.remove(0) == s {
                    Some(pd_)
                } else {
                    None
                }
            }
            Operation::Replace(ref s, ref s_) => {
                let mut pd_ = pd.clone();
                if pd_.is_empty() || &pd_.remove(0) == s {
                    pd_.insert(0, s_.clone());
                    Some(pd_)
                } else {
                    None
                }
            }
        };
        if let Some(pd_) = succ.as_mut() {
            pd_.truncate(cap);
        }
        succ
    }
}

impl<T, W> PushDownAutomaton<T, W>
where
    T: Hash + Eq + Clone,
    W: Copy + Ord,
{
    /// Creates a deterministic `PushDownAutomaton` using a sequence of transisitons.
    /// If there are multiple transitions from the same state with the same label,
    /// the last one will be used.
    pub fn new<Q, S>(oarcs: Vec<PushDownArc<T, Q, S, W>>, oinitial: Q, ofinals: Vec<Q>) -> Self
    where
        Q: Hash + Eq + Clone,
        S: Hash + Eq + Clone,
    {
        let mut labels = HashIntegeriser::new();
        let mut pd_symbols = HashIntegeriser::new();
        let mut states = HashIntegeriser::new();

        let initial = states.integerise(oinitial);

        let mut arcs = Vec::new();
        for PushDownArc {
            from,
            to,
            label,
            op,
            weight,
        } in oarcs
        {
            let iop = match op {
                Operation::Nothing => Operation::Nothing,
                Operation::Add(s) => Operation::Add(pd_symbols.integerise(s)),
                Operation::Remove(s) => Operation::Remove(pd_symbols.integerise(s)),
                Operation::Replace(s, s_) => {
                    Operation::Replace(pd_symbols.integerise(s), pd_symbols.integerise(s_))
                }
            };
            vec_entry::<IntMap<(usize, W, Operation<usize>)>>(&mut arcs, states.integerise(from))
                .insert(
                    labels.integerise(label),
                    (states.integerise(to), weight, iop),
                );
        }

        let finals = ofinals
            .into_iter()
            .filter_map(|q| states.find_key(&q))
            .collect();

        PushDownAutomaton {
            initial,
            finals,
            arcs,
            labels: Rc::new(labels),
        }
    }

    /// Computes a deterministic `FiniteAutomaton` that accepts a superset
    /// of the `PushDownAutomaton`s language.
    /// The approximation limits each push-down of a run to a given depth.
    /// The set of all resulting runs is used to read off a set of `FiniteArcs`.
    pub fn approximate(self, depth: usize) -> FiniteAutomaton<T, W> {
        let mut new_states = HashIntegeriser::new();

        let mut agenda: Vec<FiniteArc<(usize, Vec<usize>), usize, W>> = Vec::new();
        for (label, &(to, weight, ref op)) in self.arcs.get(self.initial).unwrap_or(&IntMap::default()) {
            let pd = Vec::new();
            if let Some(pd_) = op.apply_with_capacity(&pd, depth) {
                agenda.push(FiniteArc {
                    label: *label,
                    from: (self.initial, pd),
                    to: (to, pd_),
                    weight,
                })
            }
        }

        let transitions = Search::unweighted(
            agenda,
            |&FiniteArc { to: (from, ref pd), .. }| {
                let mut succ = Vec::new();
                for (label, &(to, weight, ref op)) in self.arcs.get(from).unwrap_or(&IntMap::default()) {
                    if let Some(pd_) = op.apply_with_capacity(pd, depth) {
                        succ.push(FiniteArc {
                            label: *label,
                            weight,
                            from: (from, pd.clone()),
                            to: (to, pd_),
                        });
                    }
                }
                succ
            },
        ).map(
            |FiniteArc { from, to, label, weight }| {
                FiniteArc {
                    from: new_states.integerise(from),
                    to: new_states.integerise(to),
                    label,
                    weight,
                }
            },
        ).collect();

        let new_finals: Vec<usize> = self.finals
            .into_iter()
            .filter_map(|q| new_states.find_key(&(q, Vec::new())))
            .collect();

        FiniteAutomaton::from_integerized(
            transitions,
            new_states.find_key(&(self.initial, Vec::new())).unwrap(),
            new_finals,
            self.labels,
        )
    }
}

impl<T> PushDownAutomaton<T, LogDomain<f64>>
where
    T: Hash + Eq + Clone,
{
    /// Computes the Hadamard product of a deterministic `PushDownAutomaton` and
    /// a deterministic `FiniteAutomaton`.
    pub fn intersect<W>(self, other: &FiniteAutomaton<T, W>) -> Self {
        let mut agenda: Vec<PushDownArc<usize, (usize, usize), usize, LogDomain<f64>>> = Vec::new();
        for (label, &(to, weight, op)) in self.arcs.get(self.initial).unwrap_or(&IntMap::default()) {
            if let Some(&(fto, _)) = other.arcs.get(other.initial).and_then(|m| m.get(label)) {
                agenda.push(PushDownArc {
                    from: (self.initial, other.initial),
                    to: (to, fto),
                    label: *label,
                    weight: weight,
                    op: op,
                });
            }
        }

        let mut new_arcs = Vec::new();
        let mut states = HashIntegeriser::new();
        for PushDownArc {
            from,
            to,
            label,
            weight,
            op,
        } in Search::unweighted(agenda, |arc| {
            let mut succ = Vec::new();
            let &PushDownArc { to: (kq, fq), .. } = arc;
            
            for (label, &(to, weight, op)) in self.arcs.get(kq).unwrap_or(&IntMap::default()) {
                if let Some(&(fto, _)) = other.arcs.get(fq).and_then(|m| m.get(label)) {
                    succ.push(PushDownArc {
                        from: (kq, fq),
                        to: (to, fto),
                        label: *label,
                        op: op,
                        weight,
                    });
                }
            }
            
            succ
        }).uniques()
        {
            vec_entry::<IntMap<(usize, LogDomain<f64>, Operation<usize>)>>(
                &mut new_arcs,
                states.integerise(from),
            ).insert(label, (states.integerise(to), weight, op));
        }

        let mut new_finals: Vec<usize> = Vec::new();
        for kq in self.finals {
            for fq in &other.finals {
                if let Some(q) = states.find_key(&(kq, *fq)) {
                    new_finals.push(q);
                }
            }
        }

        PushDownAutomaton {
            arcs: new_arcs,
            initial: states.find_key(&(self.initial, other.initial)).unwrap(),
            finals: new_finals,
            labels: self.labels,
        }
    }

    /// Creates an `Iterator` over all words accepted by a `PushDownAutomaton`.
    pub fn generate<'a>(self, beamwidth: Capacity) -> Box<Iterator<Item = Vec<T>> + 'a>
    where
        T: 'a,
    {
        let heuristics = self.heuristics();

        let PushDownAutomaton {
            arcs,
            initial,
            finals,
            labels,
        } = self;


        Box::new(
            Search::weighted(
                vec![(LogDomain::one(), initial, vec![], vec![])],
                move |&(weight_, q, ref word, ref pd)| {
                    let mut results = Vec::new();
                    if let Some(arcs_from) = arcs.get(q) {
                        for (label, &(to, weight, ref op)) in arcs_from {
                            if let Some(pd_) = op.apply(pd) {
                                let mut word_ = word.clone();
                                word_.push(*label);
                                results.push((weight * weight_, to, word_, pd_));
                            }
                        }
                    }
                    results
                },
                Box::new(move |&(weight, ref q, _, ref pd)| {
                    let h = pd.iter()
                        .map(|pds| {
                            heuristics
                                .get(q)
                                .and_then(|&(ref m, _)| m.get(pds).cloned())
                                .unwrap_or_else(LogDomain::zero)
                        })
                        .min()
                        .unwrap_or(
                            heuristics
                                .get(q)
                                .map(|&(_, w)| w)
                                .unwrap_or_else(LogDomain::zero),
                        );
                    (h * weight).pow(-1.0)
                }),
            ).beam(beamwidth)
                .filter(move |&(_, ref q, _, ref pd)| {
                    finals.contains(q) && pd.is_empty()
                })
                .map(move |(_, _, word, _)| {
                    word.into_iter()
                        .map(|i| labels.find_value(i).unwrap())
                        .cloned()
                        .collect()
                }),
        )
    }

    fn heuristics(&self) -> IntMap<(IntMap<LogDomain<f64>>, LogDomain<f64>)> {
        let starts = self.finals.iter().map(|q| {
            WeightedSearchItem((*q, BTreeSet::new()), LogDomain::one())
        });

        let mut bwtransitions: Vec<Vec<(usize, LogDomain<f64>, Operation<usize>)>> = Vec::new();
        for (from, farcs) in self.arcs.iter().enumerate() {
            for &(to, weight, op) in farcs.values() {
                vec_entry(&mut bwtransitions, to).push((from, weight, op));
            }
        }

        let mut heuristics = IntMap::default();
        let mut removeables_per_state = IntMap::default();
        for WeightedSearchItem((q, removeables), w) in Search::weighted(
            starts,
            |&WeightedSearchItem((q, ref removeables), w)| {
                let olds = removeables_per_state.entry(q).or_insert_with(BTreeSet::new);
                let news: BTreeSet<usize> = removeables.difference(olds).cloned().collect();
                olds.extend(news.iter().cloned());

                let mut succ = Vec::new();
                for &(from, weight, op) in bwtransitions.get(q).unwrap_or(&Vec::new()) {
                    let mut removeables_ = news.clone();
                    match op {
                        Operation::Remove(i) | Operation::Replace(i, _) => {
                            removeables_.insert(i);
                        }
                        _ => (),
                    }
                    succ.push(WeightedSearchItem((from, removeables_), w * weight));
                }
                succ
            },
            Box::new(|&WeightedSearchItem(_, w)| w.pow(-1.0)),
        ).uniques()
        {
            let map = heuristics.entry(q).or_insert((IntMap::default(), w));
            for rem in removeables {
                map.0.entry(rem).or_insert(w);
            }
        }
        heuristics
    }
}

use super::GeneratorAutomaton;
impl<T> GeneratorAutomaton<T> for PushDownAutomaton<T, LogDomain<f64>>
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
        self.clone().intersect(&other)
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

/// This tuple type is used to exploit the implementation of
/// `Serialize` and `Deserialize` for tuples.
/// It holds the conents of `PushDownAutomaton`.
type SerializedRepresentation<T, W> = (
    usize,
    Vec<usize>,
    Vec<IntMap<(usize, W, Operation<usize>)>>,
    HashIntegeriser<T>,
);

impl<T, W> Serialize for PushDownAutomaton<T, W>
where
    T: Serialize + Hash + Eq + Clone,
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

impl<'de, T, W> Deserialize<'de> for PushDownAutomaton<T, W>
where
    T: Deserialize<'de> + Hash + Eq + Clone,
    W: Deserialize<'de>,
{
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let (initial, finals, arcs, labels) = SerializedRepresentation::deserialize(d)?;
        Ok(PushDownAutomaton {
            initial,
            finals,
            arcs,
            labels: Rc::new(labels),
        })
    }
}

use std::fmt::{Display, Error, Formatter};

impl<T, W> Display for PushDownAutomaton<T, W>
where
    T: Display + Hash + Eq + Clone,
    W: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let mut buffer = String::new();
        
        for (from, arcs_from) in self.arcs.iter().enumerate() {
            for (label, &(ref to, ref weight, ref op)) in arcs_from {
                buffer.push_str(&format!(
                    "{} → [{}] {} / {:?} # {}\n",
                    from,
                    self.labels.find_value(*label).unwrap(),
                    to,
                    op,
                    weight
                ));
            }
        }
        
        write!(f, "initial: {}, finals: {:?}\n{}", &self.initial, &self.finals, &buffer)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn pda() {
        let arcs = vec![
            PushDownArc {
                from: 0,
                to: 0,
                label: "A",
                op: Operation::Add(0),
                weight: LogDomain::new(1.0).unwrap(),
            },
            PushDownArc {
                from: 0,
                to: 1,
                label: "B",
                op: Operation::Remove(0),
                weight: LogDomain::new(0.5).unwrap(),
            },
            PushDownArc {
                from: 1,
                to: 1,
                label: "B",
                op: Operation::Remove(0),
                weight: LogDomain::new(1.0).unwrap(),
            },
            PushDownArc {
                from: 0,
                to: 0,
                label: "C",
                op: Operation::Add(1),
                weight: LogDomain::new(1.0).unwrap(),
            },
        ];

        eprintln!(
            "{:?}",
            PushDownAutomaton::new(arcs, 0, vec![1]).heuristics()
        );
    }

}
