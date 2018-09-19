use grammars::pmcfg::{PMCFGRule, VarT};

use std::{ hash::Hash, collections::HashSet, fmt::Debug };
use integeriser::{HashIntegeriser, Integeriser};
use util::{IntMap, IntSet, search::Search};
use fnv::{FnvHashMap, FnvHashSet};

type RuleId = usize;
type CachedNt = usize;

/// Serializable filter for grammar rules.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedFilterPersistentStorage<T>
where
    T: Eq + Hash
{
    // key of initial nonterminal symbol
    initial_nt: usize,
    // rules without rhs nts and terminals
    free_rules: IntSet,
    // lhs nonterminals of those rules
    free_nts: IntSet,
    
    // rules with nts on rhs, no terminals
    only_nt: IntMap<Vec<(Vec<CachedNt>, RuleId, CachedNt)>>,
    // rules with no nts on rhs, but terminals
    only_t: FnvHashMap<T, Vec<(Vec<T>, RuleId, CachedNt)>>,
    // rules with nts and terminals
    nt_and_t: FnvHashMap<T, Vec<(Vec<T>, Vec<CachedNt>, RuleId, CachedNt)>>
}

impl<T> CachedFilterPersistentStorage<T>
where
    T: Clone + Eq + Hash,
{
    pub fn new<'a, N, W>(rules: impl Iterator<Item=(usize, &'a PMCFGRule<N, T, W>)> + 'a, initial: &N) -> Self 
    where
        N: Debug + Eq + Hash + 'a,
        T: 'a,
        W: 'a
    {
        let mut integeriser: HashIntegeriser<&N> = HashIntegeriser::new();
        let mut free_rules: IntSet = IntSet::default();
        let mut free_nts: IntSet = IntSet::default();

        let mut only_nt = IntMap::default();
        let mut only_t = FnvHashMap::default();
        let mut nt_and_t = FnvHashMap::default();

        for (rid, rule) in rules {
            let nid = integeriser.integerise(&rule.head);
            let mut ts = (&rule.composition).into_iter()
                               .flat_map(|v| v)
                               .filter_map(|vart| if let &VarT::T(ref t) = vart { Some(t.clone()) } else { None })
                               .collect::<HashSet<_>>()
                               .into_iter()
                               .collect::<Vec<_>>();
            if rule.tail.is_empty() {
                if ts.is_empty()  {
                    free_rules.insert(rid);
                    free_nts.insert(nid);
                } else {
                    only_t.entry(ts.remove(0))
                          .or_insert_with(Vec::new)
                          .push((ts, rid, nid));
                }
            } else {
                let i_rhs: Vec<usize> = rule.tail.iter().map(|i| integeriser.integerise(i))
                                            .collect::<HashSet<usize>>().into_iter().collect();
                if ts.is_empty()  {
                    for rhs_index in 0..(i_rhs.len()) {
                        let mut rhs_remainder = i_rhs.clone();
                        only_nt.entry(rhs_remainder.remove(rhs_index))
                               .or_insert_with(Vec::new)
                               .push((rhs_remainder, rid, nid));
                    }
                } else {
                    nt_and_t.entry(ts.remove(0))
                            .or_insert_with(Vec::new)
                            .push((ts, i_rhs, rid, nid));
                }
            }
        }

        let initial_nt = integeriser.find_key(&initial).expect(&format!("no rules with initial nonterminal {:?}", initial));

        CachedFilterPersistentStorage {
            initial_nt,
            free_rules,
            free_nts,
            only_nt,
            only_t,
            nt_and_t
        }
    }

    pub fn instantiate(&self, word: &[T]) -> IntSet {
        let ts: FnvHashSet<&T> = word.iter().collect::<FnvHashSet<_>>();

        // ## step 1: search for productive nonterminals and rules

        let mut productive_nts = self.free_nts.clone();
        let mut only_nt = self.only_nt.clone();

        let mut productive_rules: IntMap<Vec<(Vec<usize>, usize)>> = IntMap::default();

        for t in &ts {
            for &(ref ts_to_check, rid, nid) in self.only_t.get(*t).into_iter().flat_map(|v| v) {
                if ts_to_check.iter().all(|symbol| ts.contains(&symbol)) {
                    productive_nts.insert(nid);
                    productive_rules.entry(nid)
                        .or_insert_with(Vec::new)
                        .push((Vec::new(), rid));
                }
            }
            for &(ref ts_to_check, ref nts, rid, nid) in self.nt_and_t.get(*t).into_iter().flat_map(|v| v) {
                if ts_to_check.iter().all(|symbol| ts.contains(&symbol)) {
                    for nts_index in 0..(nts.len()) {
                        let mut nts_without_index = nts.clone();
                        let nts_at_index = nts_without_index.remove(nts_index);
                        only_nt.entry(nts_at_index)
                            .or_insert_with(Vec::new)
                            .push((nts.clone(), rid, nid));
                    }
                }
            }
        }

        let mut runtime_stack = productive_nts.iter().cloned().collect::<Vec<_>>();

        while let Some(nt) = runtime_stack.pop() {
            for &(ref rhs, rule, lhs) in only_nt.get(&nt).into_iter().flatten() {
                if rhs.iter().all(|a| productive_nts.contains(a)) {
                    productive_rules.entry(lhs).or_insert_with(Vec::new).push((rhs.clone(), rule));
                    if productive_nts.insert(lhs) {
                        runtime_stack.push(lhs);
                    }
                }
            }
        }

        // ## step 2: search for reachable nonterminals and rules among the productive ones

        let mut productive_and_reachable_rules: IntSet = IntSet::default();

        {
            // search for reachable nonterminals in productive_nonterminals while adding
            // reachable rules to productive_and_reachable_rules
            let s = Search::unweighted(
                vec![self.initial_nt],
                |nt| {
                    let mut nts = Vec::new();
                    for (mut new_nts, r) in productive_rules.remove(nt).into_iter().flatten() {
                        nts.append(&mut new_nts);
                        productive_and_reachable_rules.insert(r);
                    }
                    nts
                }
            );

            // execute the search
            let _: Vec<_> = s.collect();
        }

        productive_and_reachable_rules
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use grammars::pmcfg::Composition;
    use integeriser::{HashIntegeriser, Integeriser};

    #[test]
    fn filter() {
        let rules = vec![
            PMCFGRule{ head: "A", tail: vec!["A"], weight: 1f64, composition: Composition::from(vec![vec![VarT::T(1), VarT::Var(0,0)], vec![VarT::Var(0, 1), VarT::T(2)]])},
            PMCFGRule{ head: "A", tail: vec!["B"], weight: 1f64, composition: Composition::from(vec![vec![VarT::T(1), VarT::Var(0,0)], vec![VarT::Var(0, 1), VarT::T(2)]])},
            PMCFGRule{ head: "A", tail: vec![], weight: 1f64, composition: Composition::from(vec![vec![VarT::T(0)], vec![VarT::T(0)]])},
            PMCFGRule{ head: "B", tail: vec![], weight: 1f64, composition: Composition::from(vec![vec![VarT::T(3)], vec![VarT::T(3)]])},
            PMCFGRule{ head: "C", tail: vec![], weight: 1f64, composition: Composition::from(vec![vec![VarT::T(3)], vec![VarT::T(3)]])}
        ];
        
        let mut integeriser = HashIntegeriser::new();
        
        for rule in rules {
            integeriser.integerise(rule);
        }

        let filter = CachedFilterPersistentStorage::new(integeriser.values().iter().enumerate(), &"A");

        assert_eq!(filter.clone().instantiate(&[]), vec![].into_iter().collect::<IntSet>());
        assert_eq!(filter.clone().instantiate(&[1]), vec![].into_iter().collect::<>());
        assert_eq!(filter.clone().instantiate(&[2]), vec![].into_iter().collect::<>());
        assert_eq!(filter.clone().instantiate(&[1, 2]), vec![].into_iter().collect::<>());
        assert_eq!(filter.clone().instantiate(&[0, 1]), vec![2].into_iter().collect::<>());
        assert_eq!(filter.clone().instantiate(&[0, 1, 2]), vec![0, 2].into_iter().collect::<>());
        assert_eq!(filter.clone().instantiate(&[1, 2, 3]), vec![0, 1, 3].into_iter().collect::<>());
        assert_eq!(filter.clone().instantiate(&[0, 1, 2, 3]), vec![0, 1, 2, 3].into_iter().collect::<>());
    }
}
