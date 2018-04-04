use serde::Serialize;
use util::agenda::Capacity;
use std::hash::Hash;
use integeriser::{HashIntegeriser, Integeriser};
use std::collections::BTreeMap;

use pmcfg::PMCFGRule;
use log_domain::LogDomain;
use dyck;

pub mod automata;
pub mod bracket_fragment;
mod rule_fragments;

use self::automata::*;
use self::bracket_fragment::BracketFragment;
use dyck::Bracket;
use super::Lcfrs;

use std::fmt::{Display, Error, Formatter};
use util::with_time;
use util::tree::GornTree;

use dyck::multiple::MultipleDyckLanguage;
mod mdl;

/// The indices of a bracket in a CS representation for MCFG.
/// Assumes integerized rules.
#[derive(PartialEq, Eq, Hash, Clone, Debug, PartialOrd, Ord, Serialize, Deserialize)]
pub enum BracketContent<T> {
    Terminal(T),
    Component(usize, usize),
    Variable(usize, usize, usize),
}

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

/// A CS representation of an LCFRS contains a `GeneratorAutomaton`, a ´FilterAutomaton`,
/// and a `MultipleDyckLanguage` over the same bracket alphabet.
#[derive(Debug, Serialize, Deserialize)]
pub struct CSRepresentation<N, T, F, S>
where
    N: Ord + Hash + Clone,
    T: Ord + Hash + Clone,
    F: for<'a> FilterAutomaton<'a, T> + Serialize,
    S: GeneratorStrategy<T>,
{
    generator: S::Generator,
    rules: HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>,
    filter: F,
    checker: MultipleDyckLanguage<BracketContent<T>>
}

use self::automata::GeneratorStrategy;

impl<N, T, F, S> CSRepresentation<N, T, F, S>
where
    N: Ord + Hash + Clone,
    T: Ord + Hash + Clone,
    F: for<'a> FilterAutomaton<'a, T> + Serialize,
    S: GeneratorStrategy<T>,
{
    /// Instantiates a CS representation for an `LCFRS` and `GeneratorStrategy`.
    pub fn new<M>(strategy: S, grammar: M) -> Self
    where
        M: Into<Lcfrs<N, T, LogDomain<f64>>>,
    {
        let (rules, initial) = grammar.into().destruct();
        let mut irules = HashIntegeriser::new();
        for rule in rules {
            irules.integerise(rule);
        }

        let checker = mdl::mdl(irules.values(), &irules);
        let gen = strategy.create_generator_automaton(irules.values(), initial, &irules);
        let fil = F::new(irules.values().iter(), &irules, &gen);
        CSRepresentation {
            generator: gen,
            filter: fil,
            rules: irules,
            checker
        }
    }

    /// Produces a `CSGenerator` for a Chomsky-Schützenberger characterization and a `word`.
    pub fn generate(&self, word: &[T], beam: Capacity) -> CSGenerator<T, N> {
        let f = self.filter.fsa(word, &self.generator);
        let g = self.generator.intersect(f).generate(beam);

        CSGenerator {
            candidates: g,
            rules: &self.rules,
            checker: &self.checker
        }
    }

    /// Produces additional output to stderr that logs construction times and the parsing time.
    pub fn debug(&self, word: &[T], beam: Capacity) {
        let (f, filter_const) = with_time(|| self.filter.fsa(word, &self.generator));
        let filter_size = f.arcs.iter().flat_map(|map| map.values()).count();

        let (g_, intersection_time) = with_time(|| self.generator.intersect(f));
        let intersection_size = g_.size();

        eprint!(
            "{} {} {} {} {} {} {}",
            self.rules.size(),
            word.len(),
            filter_const.num_nanoseconds().unwrap(),
            filter_size,
            self.generator.size(),
            intersection_time.num_nanoseconds().unwrap(),
            intersection_size
        );

        let (cans, ptime) = with_time(|| {
            match g_.generate(beam)
                .enumerate()
                .map(|(i, frag)| (i, BracketFragment::concat(frag)))
                .filter(|&(_, ref candidate)| dyck::recognize(candidate))
                .filter_map(|(i, candidate)| toderiv(&self.rules, &candidate).map(|_| (i + 1)))
                .next()
            {
                Some(i) => i, // valid candidate
                None => 0,    // failed
            }
        });

        eprintln!(" {} {}", cans, ptime.num_nanoseconds().unwrap());
    }
}

fn toderiv<'a, 'b, N, T, I>(rules: &'a I, word: &'b [Delta<T>]) -> Option<GornTree<&'a PMCFGRule<N, T, LogDomain<f64>>>>
where
    I: Integeriser<Item=PMCFGRule<N, T, LogDomain<f64>>>,
    T: PartialEq
{
    let mut tree = BTreeMap::new();
    let mut pos = Vec::new();
    
    for sigma in word {
        match *sigma {
            dyck::Bracket::Open(BracketContent::Component(rule_id, _)) => {
                let rule_at_pos = tree.entry(pos.clone()).or_insert(rule_id);
                if rule_at_pos != &rule_id {
                    return None;
                }
            }
            dyck::Bracket::Open(BracketContent::Variable(_, i, _)) => {
                pos.push(i);
            }
            dyck::Bracket::Close(BracketContent::Variable(_, _, _)) => {
                pos.pop();
            }
            _ => (),
        }
    }

    Some(
        tree.into_iter()
            .map(|(pos, i)| (pos, rules.find_value(i).unwrap()))
            .collect(),
    )
}

/// Iterates Dyck words that represent a derivation for a word according to
/// the Chomsky-Schützenberger characterization of an MCFG.
pub struct CSGenerator<'a, T, N>
where
    T: 'a + PartialEq + Hash + Clone + Eq + Ord,
    N: 'a + Hash + Eq,
{
    candidates: Box<Iterator<Item = Vec<BracketFragment<T>>> + 'a>,
    rules: &'a HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>,
    checker: &'a MultipleDyckLanguage<BracketContent<T>>
}

impl<'a, N, T> Iterator for CSGenerator<'a, T, N>
where
    T: PartialEq + Hash + Clone + Eq + Ord,
    N: Hash + Eq + Clone,
{
    type Item = GornTree<&'a PMCFGRule<N, T, LogDomain<f64>>>;

    fn next(&mut self) -> Option<GornTree<&'a PMCFGRule<N, T, LogDomain<f64>>>> {
        let &mut CSGenerator {
            ref mut candidates,
            rules,
            checker
        } = self;
        for fragments in candidates {
            let candidate: Vec<Delta<T>> = BracketFragment::concat(fragments).into_iter().filter(|b| match *b {
                Bracket::Open(BracketContent::Terminal(_)) | Bracket::Close(BracketContent::Terminal(_)) => false,
                _ => true
            }).collect();
            if checker.recognize(&candidate) {
                if let Some(derivation) = toderiv(rules, &candidate) {
                    return Some(derivation);
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod test {
    use pmcfg::VarT;
    use pmcfg::PMCFGRule;
    use pmcfg::Composition;
    use super::Capacity;
    use super::CSRepresentation;
    use super::LogDomain;
    use super::Lcfrs;
    use super::automata::{NaiveFilterAutomaton, PushDownGenerator};

    #[test]
    fn csrep() {
        let grammar = lcfrs();
        let d1 = vec![(vec![], &grammar.rules[1])].into_iter().collect();
        let d2 = vec![
                    (vec![], &grammar.rules[0]),
                    (vec![0], &grammar.rules[1]),
                    (vec![1], &grammar.rules[1]),
                ].into_iter().collect();

        let cs =
            CSRepresentation::<&str, char, NaiveFilterAutomaton<char>, PushDownGenerator>::new(
                PushDownGenerator,
                grammar.clone(),
            );
        assert_eq!(cs.generate(&['A'], Capacity::Infinite).next(), Some(d1));
        assert_eq!(
            cs.generate(&['A', 'A'], Capacity::Infinite).next(),
            Some(d2)
        );
    }

    fn lcfrs() -> Lcfrs<&'static str, char, LogDomain<f64>> {
        Lcfrs {
            init: "S",
            rules: vec![
                PMCFGRule {
                    head: "S",
                    tail: vec!["S", "S"],
                    composition: Composition {
                        composition: vec![vec![VarT::Var(0, 0), VarT::Var(1, 0)]],
                    },
                    weight: LogDomain::new(0.3f64).unwrap(),
                },
                PMCFGRule {
                    head: "S",
                    tail: vec![],
                    composition: Composition {
                        composition: vec![vec![VarT::T('A')]],
                    },
                    weight: LogDomain::new(0.7f64).unwrap(),
                },
            ],
        }
    }
}
