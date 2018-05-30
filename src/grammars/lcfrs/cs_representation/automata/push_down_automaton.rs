use std::collections::BTreeSet;
use std::hash::Hash;

use num_traits::{One, Zero};
use integeriser::{HashIntegeriser, Integeriser};

use util::search::{Search, WeightedSearchItem};
use super::{StateInstruction, FiniteAutomaton};
use std::rc::Rc;
use util::agenda::Capacity;
use util::{vec_entry, IntMap};

use recognisable::Transition;
use std::ops::Mul;

/// An operation on a push-down.
/// The set of ops is limited to removal, addition and replacement of a single symbol.
#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug, Serialize, Deserialize, Copy)]
pub enum PushDownInstruction<S> {
    Nothing,
    Remove(S),
    Add(S),
    Replace(S, S),
}

type PDSInstruction<Q, S> = (StateInstruction<Q>, PushDownInstruction<S>);
type PDSTransition<Q, S, T, W> = Transition<PDSInstruction<Q, S>, T, W>;

use recognisable::Instruction;
impl<Q: Clone + PartialEq, S: Copy + PartialEq> Instruction
    for (StateInstruction<Q>, PushDownInstruction<S>) {
    type Storage = (Q, Vec<S>);

    fn apply(&self, s: (Q, Vec<S>)) -> Vec<(Q, Vec<S>)> {
        let (q, pd) = s;
        self.0
            .apply(q)
            .into_iter()
            .filter_map(|q| self.1.apply(&pd).map(|pd| (q, pd)))
            .collect()
    }
}

/// A deterministic push-down `PushDownAutomaton`.
#[derive(Debug, Clone)]
pub struct PushDownAutomaton<T, W>
where
    T: Hash + Eq + Clone,
{
    pub qi: usize,
    pub qf: usize,
    pub arcs: Vec<IntMap<(usize, W, PushDownInstruction<usize>)>>,
    pub labels: Rc<HashIntegeriser<T>>,
}

impl<S> PushDownInstruction<S>
where
    S: Copy + PartialEq,
{
    /// Applies a `PushDownInstruction` to a push-down
    /// and returns a push-down if it succeeds.
    /// The `Clone` is only performed if the operation
    /// is applicable.
    fn apply(&self, pd: &Vec<S>) -> Option<Vec<S>> {
        match *self {
            PushDownInstruction::Nothing => Some(pd.clone()),
            PushDownInstruction::Add(s) => {
                let mut pd_ = pd.clone();
                pd_.push(s);
                Some(pd_)
            }
            PushDownInstruction::Remove(s) => {
                pd.split_last().and_then(|(&s_, f)| if s_ == s {
                    Some(f.to_vec())
                } else {
                    None
                })
            }
            PushDownInstruction::Replace(s, s_) => {
                pd.split_last().and_then(|(&vs, f)| if vs == s {
                    let mut pd_ = f.to_vec();
                    pd_.push(s_);
                    Some(pd_)
                } else {
                    None
                })
            }
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
            PushDownInstruction::Nothing => Some(pd.clone()),
            PushDownInstruction::Add(ref s) => {
                let mut pd_ = pd.clone();
                pd_.insert(0, s.clone());
                Some(pd_)
            }
            PushDownInstruction::Remove(ref s) => {
                let mut pd_ = pd.clone();
                if pd_.is_empty() || &pd_.remove(0) == s {
                    Some(pd_)
                } else {
                    None
                }
            }
            PushDownInstruction::Replace(ref s, ref s_) => {
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
{
    /// Creates a deterministic `PushDownAutomaton` using a sequence of transisitons.
    /// If there are multiple transitions from the same state with the same label,
    /// the last one will be used.
    pub fn new<Q, S>(oarcs: Vec<PDSTransition<Q, S, T, W>>, oinitial: Q, ofinal: Q) -> Self
    where
        Q: Hash + Eq + Clone,
        S: Hash + Eq + Clone,
        W: Clone
    {
        let mut labels = HashIntegeriser::new();
        let mut pd_symbols = HashIntegeriser::new();
        let mut states = HashIntegeriser::new();

        let qi = states.integerise(oinitial);

        let mut arcs = Vec::new();
        for Transition {
            instruction: (StateInstruction(from, to), op),
            mut word,
            weight,
        } in oarcs
        {
            debug_assert!(word.len() == 1);

            // Integerize instruction
            let iop = match op {
                PushDownInstruction::Nothing => PushDownInstruction::Nothing,
                PushDownInstruction::Add(s) => PushDownInstruction::Add(pd_symbols.integerise(s)),
                PushDownInstruction::Remove(s) => PushDownInstruction::Remove(
                    pd_symbols.integerise(s),
                ),
                PushDownInstruction::Replace(s, s_) => {
                    PushDownInstruction::Replace(
                        pd_symbols.integerise(s),
                        pd_symbols.integerise(s_),
                    )
                }
            };

            vec_entry::<IntMap<(usize, W, PushDownInstruction<usize>)>>(
                &mut arcs,
                states.integerise(from),
            ).insert(labels.integerise(word.remove(0)), (
                states.integerise(to),
                weight,
                iop,
            ));
        }

        PushDownAutomaton {
            qi,
            qf: states.integerise(ofinal),
            arcs,
            labels: Rc::new(labels),
        }
    }

    /// Computes a deterministic `FiniteAutomaton` that accepts a superset
    /// of the `PushDownAutomaton`s language.
    /// The approximation limits each push-down of a run to a given depth.
    /// The set of all resulting runs is used to read off a set of `FiniteArcs`.
    pub fn approximate(self, depth: usize) -> FiniteAutomaton<T, W> where W: Copy {
        let mut new_states = HashIntegeriser::new();

        let mut agenda = Vec::new();
        for (label, &(to, weight, ref op)) in
            self.arcs.get(self.qi).unwrap_or(&IntMap::default())
        {
            let pd = Vec::new();
            if let Some(pd_) = op.apply_with_capacity(&pd, depth) {
                agenda.push(
                    WeightedSearchItem((self.qi, pd, *label, pd_, to), weight)
                );
            }
        }

        let transitions = Search::unweighted(agenda, |&WeightedSearchItem((_, _, _, ref pd, from), _)| {
            let mut succ = Vec::new();
            for (label, &(to, weight, ref op)) in
                self.arcs.get(from).unwrap_or(&IntMap::default())
            {
                if let Some(pd_) = op.apply_with_capacity(pd, depth) {
                    succ.push(
                        WeightedSearchItem((from, pd.clone(), *label, pd_, to), weight)
                    );
                }
            }
            succ
        }).uniques()
            .map(|WeightedSearchItem((q, pd, s, pd_, q_), weight)| {
                Transition {
                    instruction: StateInstruction(
                        new_states.integerise((q, pd)),
                        new_states.integerise((q_, pd_)),
                    ),
                    word: vec![s],
                    weight,
                }
            })
            .collect();

        FiniteAutomaton::from_integerized(
            transitions,
            new_states.find_key(&(self.qi, Vec::new())).unwrap(),
            new_states.integerise((self.qf, Vec::new())),
            self.labels,
        )
    }
}

impl<T, W> PushDownAutomaton<T, W>
where
    T: Hash + Eq + Clone,
{
    /// Computes the Hadamard product of a deterministic `PushDownAutomaton` and
    /// a deterministic `FiniteAutomaton`.
    pub fn intersect(self, other: &FiniteAutomaton<T, ()>) -> Self where W: Copy {
        let mut agenda = Vec::new();

        for (label, &(to, weight, op)) in
            self.arcs.get(self.qi).unwrap_or(&IntMap::default())
        {
            if let Some(&(fto, _)) = other.arcs.get(other.qi).and_then(|m| m.get(label)) {
                agenda.push(
                    WeightedSearchItem((self.qi, other.qi, op, *label, to, fto), weight)
                );
            }
        }

        let mut new_arcs = Vec::new();
        let mut states = HashIntegeriser::new();
        for WeightedSearchItem((from1, from2, i, s, to1, to2), weight) in
            Search::unweighted(agenda, |&WeightedSearchItem((_, _, _, _, kq, fq), _)| {
                let mut succ = Vec::new();

                for (label, &(to, weight, op)) in self.arcs.get(kq).unwrap_or(&IntMap::default()) {
                    if let Some(&(fto, _)) = other.arcs.get(fq).and_then(|m| m.get(label)) {
                        succ.push(
                            WeightedSearchItem((kq, fq, op, *label, to, fto), weight)
                        );
                    }
                }

                succ
            }).uniques()
        {
            vec_entry::<IntMap<(usize, W, PushDownInstruction<usize>)>>(
                &mut new_arcs,
                states.integerise((from1, from2)),
            ).insert(s, (states.integerise((to1, to2)), weight, i));
        }

        PushDownAutomaton {
            arcs: new_arcs,
            qi: states.integerise((self.qi, other.qi)),
            qf: states.integerise((self.qf, other.qf)),
            labels: self.labels,
        }
    }

    /// Creates an `Iterator` over all words accepted by a `PushDownAutomaton`.
    pub fn generate(self, beamwidth: Capacity) -> impl Iterator<Item = Vec<T>> where W: Ord + Copy + Mul<Output=W> + One + Zero {
        let heuristics = self.heuristics();

        let PushDownAutomaton {
            arcs,
            qi,
            qf,
            labels,
        } = self;

        // empty agenda if there are no final states
        let initial_agenda = vec![
            WeightedSearchItem(
                (W::one(), qi, vec![], vec![]),
                heuristics.get(&qi).map(|t| t.1).unwrap_or_else(W::zero)
            ),
        ];

        Search::weighted(initial_agenda, move |&WeightedSearchItem((weight_,
                                       q,
                                       ref word,
                                       ref pd),
                                      _)| {
                let mut results = Vec::new();

                if let Some(arcs_from) = arcs.get(q) {
                    for (label, &(to, weight, ref op)) in arcs_from {
                        if let Some(pd_) = op.apply(pd) {
                            let mut word_ = word.clone();
                            word_.push(*label);

                            let successorweight = weight * weight_;
                            let priority = pd_.iter()
                                .map(|pds| {
                                    heuristics
                                        .get(&to)
                                        .and_then(|&(ref m, _)| m.get(pds).cloned())
                                        .unwrap_or_else(W::zero)
                                })
                                .min()
                                .unwrap_or(heuristics.get(&to).map(|&(_, w)| w).unwrap_or_else(
                                    W::zero,
                                )) * successorweight;

                            if !priority.is_zero() {
                                results.push(WeightedSearchItem(
                                    (successorweight, to, word_, pd_),
                                    priority,
                                ));
                            }
                        }
                    }
                }

                results
            }).beam(beamwidth)
                .filter(move |&WeightedSearchItem((_, ref q, _, ref pd), _)| q == &qf && pd.is_empty())
                .map(move |WeightedSearchItem((_, _, word, _), _)| {
                    word.into_iter()
                        .map(|i| labels.find_value(i).unwrap())
                        .cloned()
                        .collect()
                })
    }

    fn heuristics(&self) -> IntMap<(IntMap<W>, W)> where W: Ord + Copy + Mul<Output=W> + One {
        let starts = vec![WeightedSearchItem((self.qf, BTreeSet::new()), W::one())];

        let mut bwtransitions: Vec<
            Vec<
                (usize,
                 W,
                 PushDownInstruction<usize>),
            >,
        > = Vec::new();
        for (from, farcs) in self.arcs.iter().enumerate() {
            for &(to, weight, op) in farcs.values() {
                vec_entry(&mut bwtransitions, to).push((from, weight, op));
            }
        }

        let mut heuristics = IntMap::default();
        let mut removeables_per_state = IntMap::default();
        for WeightedSearchItem((q, removeables), w) in
            Search::weighted(starts, |&WeightedSearchItem((q, ref removeables), w)| {
                let olds = removeables_per_state.entry(q).or_insert_with(BTreeSet::new);
                let news: BTreeSet<usize> = removeables.difference(olds).cloned().collect();
                olds.extend(news.iter().cloned());

                let mut succ = Vec::new();
                for &(from, weight, op) in bwtransitions.get(q).unwrap_or(&Vec::new()) {
                    let mut removeables_ = news.clone();
                    match op {
                        PushDownInstruction::Remove(i) |
                        PushDownInstruction::Replace(i, _) => {
                            removeables_.insert(i);
                        }
                        _ => (),
                    }
                    succ.push(WeightedSearchItem((from, removeables_), w * weight));
                }
                succ
            }).uniques()
        {
            let map = heuristics.entry(q).or_insert(
                (IntMap::default(), w),
            );
            for rem in removeables {
                map.0.entry(rem).or_insert(w);
            }
        }
        heuristics
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

/// This tuple type is used to exploit the implementation of
/// `Serialize` and `Deserialize` for tuples.
/// It holds the conents of `PushDownAutomaton`.
type SerializedRepresentation<T, W> = (usize,
                                       usize,
                                       Vec<IntMap<(usize, W, PushDownInstruction<usize>)>>,
                                       HashIntegeriser<T>);

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
            &self.qi,
            &self.qf,
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
        let (qi, qf, arcs, labels) = SerializedRepresentation::deserialize(d)?;
        Ok(PushDownAutomaton {
            qi,
            qf,
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

        write!(
            f,
            "initial: {}, final: {:?}\n{}",
            &self.qi,
            &self.qf,
            &buffer
        )
    }
}

use std::ops::MulAssign;
use recognisable::automaton::Automaton;
use recognisable::Configuration;
use std::collections::{BinaryHeap, HashMap};
use recognisable::Item;
impl<T, W> Automaton<T, W> for PushDownAutomaton<T, W>
where
    T: Clone + Eq + Hash + Ord,
    W: Copy + One + MulAssign + Ord,
{
    type Key = usize;
    type I = PDSInstruction<usize, usize>;
    type IInt = PDSInstruction<usize, usize>;
    type TInt = usize;

    fn from_transitions<It>(_: It, _: <Self::I as Instruction>::Storage) -> Self
    where
        It: IntoIterator<Item = Transition<Self::I, T, W>>,
    {
        panic!("not implemented")
    }

    fn transitions<'a>(&'a self) -> Box<Iterator<Item = Transition<Self::I, T, W>> + 'a> {
        let mut v = Vec::new();
        for (from, arcs_from) in self.arcs.iter().enumerate() {
            for (isym, &(to, weight, i)) in arcs_from {
                v.push(Transition {
                    word: vec![self.labels.find_value(*isym).unwrap().clone()],
                    weight,
                    instruction: (StateInstruction(from, to), i),
                })
            }
        }

        Box::new(v.into_iter())
    }

    fn initial(&self) -> <Self::I as Instruction>::Storage {
        (self.qi, Vec::new())
    }

    /// Maps items from the internal representation to the desired output.
    fn item_map(
        &self,
        i: &Item<(usize, Vec<usize>), PDSInstruction<usize, usize>, usize, W>,
    ) -> Item<(usize, Vec<usize>), PDSInstruction<usize, usize>, T, W> {
        let &(Configuration {
                  ref word,
                  weight,
                  ref storage,
              },
              ref pd) = i;

        (
            Configuration {
                word: word.iter()
                    .map(|i| self.labels.find_value(*i).unwrap())
                    .cloned()
                    .collect(),
                weight,
                storage: storage.clone(),
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
        &c.storage.0
    }

    fn is_terminal(
        &self,
        c: &Configuration<<Self::IInt as Instruction>::Storage, Self::TInt, W>,
    ) -> bool {
        c.word.is_empty() && self.qf == c.storage.0 && c.storage.1.is_empty()
    }

    fn transition_map(
        &self,
    ) -> Rc<HashMap<usize, BinaryHeap<PDSTransition<usize, usize, usize, W>>>> {
        Rc::new(
            self.arcs
                .iter()
                .enumerate()
                .map(|(from, arcs_from)| {
                    (
                        from,
                        arcs_from
                            .iter()
                            .map(|(isymbol, &(to, weight, i))| {
                                Transition {
                                    word: vec![*isymbol],
                                    weight,
                                    instruction: (StateInstruction(from, to), i),
                                }
                            })
                            .collect(),
                    )
                })
                .collect(),
        )
    }

    /// Returns the initial storage configuration (in its internal representation).
    fn initial_int(&self) -> <Self::IInt as Instruction>::Storage {
        (self.qi, Vec::new())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use util::reverse::Reverse;
    use log_domain::LogDomain;

    #[test]
    fn heuristic() {
        let one = LogDomain::new(1.0).unwrap().into();
        let pointfive = LogDomain::new(0.5).unwrap().into();

        let h: IntMap<(IntMap<Reverse<LogDomain<f64>>>, Reverse<LogDomain<f64>>)> =
            vec![
                (1, (vec![(0, one)].into_iter().collect(), one)),
                (0, (vec![(0, pointfive)].into_iter().collect(), pointfive)),
            ].into_iter().collect();

        assert_eq!(
            example_pda().heuristics(),
            h
        );
    }

    #[test]
    fn constructor() {
        let one = LogDomain::new(1.0).unwrap().into();
        let pointfive = LogDomain::new(0.5).unwrap().into();
        let pda = example_pda();

        assert_eq!(pda.qi, 0);
        assert_eq!(pda.qf, 1);

        let arcs: Vec<IntMap<(usize, Reverse<LogDomain<f64>>, PushDownInstruction<usize>)>>
            = vec![
                vec![(0, (0, one, PushDownInstruction::Add(0))), (1, (1, pointfive, PushDownInstruction::Remove(0))), (2, (0, one, PushDownInstruction::Add(1)))].into_iter().collect(),
                vec![(1, (1, one, PushDownInstruction::Remove(0)))].into_iter().collect()
            ];

        assert_eq!(
            pda.arcs,
            arcs
        );
    }

    #[test]
    fn words() {
        let pda = example_pda();

        assert_eq!(
            pda.generate(Capacity::Infinite).take(5).collect::<Vec<_>>(),
            vec![
                vec!["A", "B"],
                vec!["A", "A", "B", "B"],
                vec!["A", "A", "A", "B", "B", "B"],
                vec!["A", "A", "A", "A", "B", "B", "B", "B"],
                vec!["A", "A", "A", "A", "A", "B", "B", "B", "B", "B"],
            ]
        )
    }

    fn example_pda() -> PushDownAutomaton<&'static str, Reverse<LogDomain<f64>>> {
        let one = LogDomain::new(1.0).unwrap().into();
        let pointfive = LogDomain::new(0.5).unwrap().into();

        let arcs: Vec<PDSTransition<usize, usize, &str, Reverse<LogDomain<f64>>>> = vec![
            Transition {
                instruction: (StateInstruction(0, 0), PushDownInstruction::Add(0)),
                word: vec!["A"],
                weight: one,
            },
            Transition {
                instruction: (StateInstruction(0, 1), PushDownInstruction::Remove(0)),
                word: vec!["B"],
                weight: pointfive,
            },
            Transition {
                instruction: (StateInstruction(1, 1), PushDownInstruction::Remove(0)),
                word: vec!["B"],
                weight: one,
            },
            Transition {
                instruction: (StateInstruction(0, 0), PushDownInstruction::Add(1)),
                word: vec!["C"],
                weight: one,
            },
        ];
        
        PushDownAutomaton::new(arcs, 0, 1)
    }

}
