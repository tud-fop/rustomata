use openfsa::fsa::{Arc, Automaton};
use log_domain::LogDomain;
use num_traits::{One, Zero};

use std::hash::Hash;
use std::collections::{BTreeSet, BinaryHeap, HashMap};
use std::rc::Rc;
use std::fmt::Debug;

use recognisable::{DeterministicSearch, Recogniser};
use util::push_down::Pushdown;
use util::agenda::{Agenda, BoundedPriorityQueue};

pub struct DFA<T>(
    usize,
    Vec<usize>,
    Vec<Arc<usize, T>>,
    HashMap<usize, LogDomain<f32>>,
);

impl<T> DFA<T>
where
    T: Hash + Eq + Clone + Debug,
{
    fn heuristics(
        arcs: &[Arc<usize, T>],
        qfs: &[usize],
    ) -> HashMap<usize, LogDomain<f32>> {
        let mut agenda: BinaryHeap<(usize, Pushdown<(LogDomain<f32>, usize)>)> = BinaryHeap::new();
        let mut avoid: BTreeSet<usize> = BTreeSet::new();
        for qf in qfs {
            agenda.push((*qf, Pushdown::new()));
            avoid.insert(*qf);
        }
        let mut rules: HashMap<usize, BinaryHeap<(LogDomain<f32>, usize)>> = HashMap::new();
        for &Arc{ ref from, ref to, ref weight, .. } in arcs {
            let rules_to = rules.entry(*to).or_insert(BinaryHeap::new());
            rules_to.push((*weight, *from));
        }
        
        let map = Recogniser {
            agenda,
            configuration_characteristic: Box::new(| q | q),
            filtered_rules: Rc::new(rules),
            apply: Box::new(| _, &(_ , ref from) | vec![*from]),
            accepting: Box::new(| _ | true),
            item_map: Box::new(| &(ref q, ref ts) | {
                let tsv: Vec<(LogDomain<f32>, usize)> = ts.clone().into();
                let w: LogDomain<f32> = tsv.into_iter().fold(LogDomain::one(), | w1, (w2, _) | w1*w2);
                (*q, w)
            }),
            already_found: Some(avoid)
        }.collect();
        map
    }


    pub fn from_fsa(automaton: Automaton<T>) -> DFA<T> {
        let (arcs, q0, qfs) = automaton.into_arcs();
        let heuristics = DFA::heuristics(&arcs, &qfs);
        
        DFA(q0, qfs, arcs, heuristics)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Item<T>(LogDomain<f32>, usize, Vec<T>);
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Trans<T>(LogDomain<f32>, usize, T);

impl<T> DFA<T>
where
    T: Ord + Clone + 'static + Debug,
{
    pub fn generate(self, beam: usize) -> Box<Iterator<Item = Vec<T>>> {
        let DFA(q0, qfs, arcs, heuristics) = self;

        let mut rules = HashMap::new();
        for Arc {
            from,
            to,
            label,
            weight,
        } in arcs
        {
            let rules_from = rules.entry(from).or_insert(BinaryHeap::new());
            rules_from.push(Trans(weight, to, label));
        }

        let mut initagenda = BoundedPriorityQueue::new(
            beam, 
            Box::new(move | &Item(ref w, ref q, _) | {
                (*heuristics.get(q).unwrap_or(&LogDomain::zero()) * *w).pow(-1.0)
            })
        );
        initagenda.enqueue(Item(LogDomain::one(), q0, vec![]));
        
        Box::new(
            DeterministicSearch {
                agenda: initagenda,
                item_to_key: Box::new(|&Item(_, ref from, _)| from),
                key_to_rules: rules,
                apply: Box::new(
                    |&Item(ref wi, _, ref word), &Trans(ref wt, ref to, ref symbol)| {
                        let mut w: Vec<T> = word.clone();
                        w.push(symbol.clone());
                        Item(*wi * *wt, *to, w)
                    },
                ),
                accepting: Box::new(move |&Item(_, ref q, _)| qfs.contains(q))
            }. map(|Item(_, _, w)| w)
        )
    }
}
