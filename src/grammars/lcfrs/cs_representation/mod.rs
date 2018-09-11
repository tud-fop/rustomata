mod fallback;
mod automaton;
mod rule_fragments;

use self::automaton::{CykAutomatonPersistentStorage, CachedFilterPersistentStorage};
use super::Lcfrs;

use dyck::Bracket;
use grammars::pmcfg::PMCFGRule;
use util::{ with_time, take_capacity, tree::GornTree, factorizable::Factorizable, agenda::Capacity };

use integeriser::{HashIntegeriser, Integeriser};
use std::{ collections::{BTreeMap}, fmt::{Display, Error, Formatter}, hash::Hash, ops::Mul };
use num_traits::{Zero, One};


/// The indices of a bracket in a CS representation for MCFG.
/// Assumes integerized rules.
#[derive(PartialEq, Eq, Hash, Clone, Debug, PartialOrd, Ord, Serialize, Deserialize)]
pub enum BracketContent<T> {
    Terminal(T),
    Component(usize, usize),
    Variable(usize, usize, usize),
}

type Delta<T> = Bracket<BracketContent<T>>;

impl<T> Display for BracketContent<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            BracketContent::Terminal(ref t) => write!(f, "_{{{}}}", t),
            BracketContent::Component(rule_id, comp_id) => write!(f, "_{}^{}", rule_id, comp_id),
            BracketContent::Variable(rule_id, i, j) => write!(f, "_{{{},{}}}^{}", rule_id, i, j),
        }
    }
}

/// A C-S representation of a grammar.
#[derive(Debug, Serialize, Deserialize)]
pub struct CSRepresentation<N, T, W>
where
    N: Ord + Hash + Clone,
    T: Ord + Hash + Clone,
    W: Ord + Clone
{
    generator: CykAutomatonPersistentStorage<BracketContent<T>, W>,
    filter: CachedFilterPersistentStorage<T>,
    rules: HashIntegeriser<PMCFGRule<N, T, W>>,
}

impl<N, T, W> CSRepresentation<N, T, W>
where
    N: Ord + Hash + Clone,
    T: Ord + Hash + Clone + ::std::fmt::Debug,
    W: Ord + Clone + ::std::fmt::Debug
{
    /// Instantiates a CS representation for an `LCFRS`.
    pub fn new<M>(grammar: M) -> Self
    where
        M: Into<Lcfrs<N, T, W>>,
        W: Copy + One + Factorizable + Mul<Output=W>
    {
        let (rules, initial) = grammar.into().destruct();
        let mut irules = HashIntegeriser::new();
        for rule in rules {
            irules.integerise(rule);
        }
        
        CSRepresentation {
            generator: CykAutomatonPersistentStorage::from_grammar(irules.values().iter(), &irules, initial),
            filter: CachedFilterPersistentStorage::new(irules.values().iter().enumerate()),
            rules: irules,
        }
    }

    /// Produces a `CSGenerator` for a Chomsky-Schützenberger characterization and a `word` in the first component.
    /// The second component contains a fallback parse tree that is built using a context-free approximation of the grammar.
    pub fn generate<'a>(&'a self, word: &[T], beam: Capacity, candidates: Capacity) -> (impl Iterator<Item=GornTree<&'a PMCFGRule<N, T, W>>> + 'a, Option<GornTree<PMCFGRule<N, T, W>>>)
    where
        W: One + Zero + Mul<Output=W> + Copy + Ord + Factorizable
    {
        let automaton = self.generator.intersect(self.filter.instantiate(word), word);
        let mut chart = match beam {
            Capacity::Limit(beam) => automaton.fill_chart_beam(beam),
            Capacity::Infinite => automaton.fill_chart(),
        }.into_iter().peekable();

        let first = chart.peek().map(|w| fallback::FailedParseTree::new(w).merge(&self.rules));
        
        ( take_capacity(chart, candidates).filter_map(move |bs| self.toderiv(&bs))
        , first
        )
    }

    /// Produces additional output to stderr that logs construction times and the parsing time.
    pub fn debug(&self, word: &[T], beam: Capacity, candidates: Capacity) -> (usize, usize, usize, i64, Option<(GornTree<PMCFGRule<N, T, W>>, usize)>)
    where
        W: One + Zero + Mul<Output=W> + Copy + Ord + Factorizable,
        T: ::std::fmt::Debug
    {
        let (f, filter_const) = with_time(|| self.filter.instantiate(word));
        let filter_size = f.len();
        let (g_, intersection_time) =  with_time(|| self.generator.intersect(f.into_iter(), word));

        let (cans, ptime) = with_time(|| {
            if candidates == Capacity::Limit(0) {
                match beam {
                    Capacity::Limit(beam) => g_.fill_chart_beam(beam),
                    Capacity::Infinite => g_.fill_chart(),
                }.into_iter().next().map(|c| (fallback::FailedParseTree::new(&c).merge(&self.rules), 0))
            }
            else {
                let mut it = take_capacity(
                    match beam {
                        Capacity::Limit(beam) => g_.fill_chart_beam(beam),
                        Capacity::Infinite => g_.fill_chart(),
                    }.into_iter().enumerate(), candidates).peekable();
                let fb = it.peek().map(|(_, w)| fallback::FailedParseTree::new(w).merge(&self.rules));
                match it.filter_map(|(i, candidate)| self.toderiv(&candidate).map(| t | (t, i + 1))).next() {
                    Some((t, i)) => Some((t.into_iter().map(|(k, v)| (k, v.clone())).collect(), i)), // valid candidate
                    None => fb.map(|t| (t, 0)),    // failed
                }
            }
        });
        
        ( self.rules.size()
        , word.len()
        , filter_size
        , filter_const.num_nanoseconds().unwrap() + intersection_time.num_nanoseconds().unwrap() + ptime.num_nanoseconds().unwrap()
        , cans
        )
    }

    /// Reads off a parse tree from a multiply Dyck word. Fails if the word is not in R ∩ D.
    fn toderiv<'a>(&'a self, word: &[Delta<T>]) -> Option<GornTree<&'a PMCFGRule<N, T, W>>> {
        let mut tree = BTreeMap::new();
        let mut pos = Vec::new();

        for sigma in word {
            match *sigma {
                Bracket::Open(BracketContent::Component(rule_id, _)) => {
                    let rule_at_pos = tree.entry(pos.clone()).or_insert(rule_id);
                    if rule_at_pos != &rule_id {
                        return None;
                    }
                }
                Bracket::Open(BracketContent::Variable(_, i, _)) => {
                    pos.push(i);
                }
                Bracket::Close(BracketContent::Variable(_, _, _)) => {
                    pos.pop();
                }
                _ => (),
            }
        }

        Some(
            tree.into_iter()
                .map(|(pos, i)| (pos, self.rules.find_value(i).unwrap()))
                .collect(),
        )
    }
}

#[cfg(test)]
mod test {
    use grammars::pmcfg::{VarT, PMCFGRule, Composition};
    use super::{Capacity, CSRepresentation, Lcfrs};
    use util::reverse::Reverse;
    use log_domain::LogDomain;

    #[test]
    fn csrep() {
        let grammar = lcfrs();
        let d1 = vec![(vec![], &grammar.rules[1])].into_iter().collect();
        let d2 = vec![
            (vec![], &grammar.rules[0]),
            (vec![0], &grammar.rules[1]),
            (vec![1], &grammar.rules[1]),
        ].into_iter()
            .collect();

        let cs = CSRepresentation::new(grammar.clone());
        assert_eq!(cs.generate(&['A'], Capacity::Infinite, Capacity::Infinite).0.next(), Some(d1));
        assert_eq!(
            cs.generate(&['A', 'A'], Capacity::Infinite, Capacity::Infinite).0.next(),
            Some(d2)
        );
    }

    fn lcfrs() -> Lcfrs<&'static str, char, Reverse<LogDomain<f64>>> {
        Lcfrs {
            init: "S",
            rules: vec![
                PMCFGRule {
                    head: "S",
                    tail: vec!["S", "S"],
                    composition: Composition {
                        composition: vec![vec![VarT::Var(0, 0), VarT::Var(1, 0)]],
                    },
                    weight: LogDomain::new(0.3f64).unwrap().into(),
                },
                PMCFGRule {
                    head: "S",
                    tail: vec![],
                    composition: Composition { composition: vec![vec![VarT::T('A')]] },
                    weight: LogDomain::new(0.7f64).unwrap().into(),
                },
            ],
        }
    }
}
