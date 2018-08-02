use grammars::pmcfg::{PMCFGRule, VarT};

use std::{ hash::Hash, collections::HashSet };

/// This is an `Iterator` that yields those grammar rules from a list that are 
/// productive and limited to a certain word.
/// Each rule is uniquely identified by an integer.
pub struct NaiveFilter<'a, I, N, T, W>
where
    I: Iterator<Item=(usize, &'a PMCFGRule<N, T, W>)> + 'a,
    N: Hash + Eq + 'a,
    T: Hash + Eq + 'a,
    W: 'a
{
    iterator: I,

    allowed_symbols: HashSet<T>,
    nonterminals: HashSet<N>,
    remaining: Vec<(usize, &'a PMCFGRule<N, T, W>)>,
    
    changed: bool,
    index: usize
}

impl<'a, I, N, T, W> NaiveFilter<'a, I, N, T, W> 
where
    I: Iterator<Item=(usize, &'a PMCFGRule<N, T, W>)> + 'a,
    N: Hash + Eq + 'a,
    T: Hash + Eq + 'a + Clone,
    W: 'a
{
    /// Constructs a new `NaiveFilter` from an iterator over rules with id and
    /// a word.
    pub fn new<II>(it: II, word: &[T]) -> Self
    where
        II: IntoIterator<IntoIter=I, Item=(usize, &'a PMCFGRule<N, T, W>)> + 'a,
    {
        NaiveFilter { 
            iterator: it.into_iter(), 
            
            nonterminals: HashSet::new(), 
            allowed_symbols: word.iter().cloned().collect(),
            
            remaining: Vec::new(),
            
            changed: false,
            index: 0
        }
    }
}

impl<'a, I, N, T, W> Iterator for NaiveFilter<'a, I, N, T, W>
where
    I: Iterator<Item=(usize, &'a PMCFGRule<N, T, W>)>,
    N: Hash + Eq + 'a + Clone,
    T: Hash + Eq + 'a,
    W: 'a
{
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        // phase 1: search for rules with rank 0
        while let Some((i, rule)) = self.iterator.next() {
            if !(&rule.composition).into_iter().flat_map(|comp| comp).any(|vart| match vart { &VarT::T(ref t) => !self.allowed_symbols.contains(t), _ => false }) {
                if rule.tail.is_empty() {
                    self.nonterminals.insert(rule.head.clone());
                    self.changed = true;
                    return Some(i)
                } else {
                    if rule.tail.iter().all(|n| self.nonterminals.contains(n)) {
                        self.nonterminals.insert(rule.head.clone());
                        self.changed = true;
                        return Some(i)
                    } else {
                        self.remaining.push((i, rule));
                    }
                }
            }
        }

        // phase 2: treat remaining rules
        // self.remaining contains only rules with terminals that occur in word
        // we search for those rules whose rhs nts are in self.nonterminals
        // until we find a fixpoint
        loop {
            if self.index == self.remaining.len() {
                if self.changed {
                    self.index = 0;
                    self.changed = false;
                } else {
                    break;
                }
            }

            while self.index < self.remaining.len() {
                if self.remaining[self.index].1.tail.iter().all(|n| self.nonterminals.contains(n)) {
                    self.nonterminals.insert(self.remaining[self.index].1.head.clone());
                    self.changed = true;
                    let (i, _) = self.remaining.remove(self.index);
                    return Some(i)
                } else {
                    self.index += 1;
                }
            }
        }

        None
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
            PMCFGRule{ head: "B", tail: vec![], weight: 1f64, composition: Composition::from(vec![vec![VarT::T(3)], vec![VarT::T(3)]])}
        ];
        
        let mut integeriser = HashIntegeriser::new();
        
        for rule in rules {
            integeriser.integerise(rule);
        }

        assert_eq!(NaiveFilter::new(integeriser.values().iter().enumerate(), &[]).collect::<HashSet<usize>>(), vec![].into_iter().collect::<HashSet<usize>>());
        assert_eq!(NaiveFilter::new(integeriser.values().iter().enumerate(), &[1]).collect::<HashSet<usize>>(), vec![].into_iter().collect::<HashSet<usize>>());
        assert_eq!(NaiveFilter::new(integeriser.values().iter().enumerate(), &[2]).collect::<HashSet<usize>>(), vec![].into_iter().collect::<HashSet<usize>>());
        assert_eq!(NaiveFilter::new(integeriser.values().iter().enumerate(), &[1, 2]).collect::<HashSet<usize>>(), vec![].into_iter().collect::<HashSet<usize>>());
        assert_eq!(NaiveFilter::new(integeriser.values().iter().enumerate(), &[0, 1]).collect::<HashSet<usize>>(), vec![2].into_iter().collect::<HashSet<usize>>());
        assert_eq!(NaiveFilter::new(integeriser.values().iter().enumerate(), &[0, 1, 2]).collect::<HashSet<usize>>(), vec![0, 2].into_iter().collect::<HashSet<usize>>());
        assert_eq!(NaiveFilter::new(integeriser.values().iter().enumerate(), &[1, 2, 3]).collect::<HashSet<usize>>(), vec![0, 1, 3].into_iter().collect::<HashSet<usize>>());
        assert_eq!(NaiveFilter::new(integeriser.values().iter().enumerate(), &[0, 1, 2, 3]).collect::<HashSet<usize>>(), vec![0, 1, 2, 3].into_iter().collect::<HashSet<usize>>());
    }
}