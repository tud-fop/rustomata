pub mod cli;

use serde::Serialize;

use std::hash::Hash;
use std::fmt;
use integeriser::{HashIntegeriser, Integeriser};
use std::collections::{BTreeMap};

use pmcfg::{PMCFG, PMCFGRule};
use log_domain::LogDomain;
use dyck;

pub mod automata;
pub mod bracket_fragment;
use cs_representation::bracket_fragment::BracketFragment;
use self::automata::*;

use std::fmt::{Display, Error, Formatter};

/// A derivation tree of PMCFG rules.
#[derive(PartialEq, Debug)]
pub struct Derivation<'a, N: 'a, T: 'a>(
    BTreeMap<Vec<usize>, &'a PMCFGRule<N, T, LogDomain<f32>>>,
);

use std::iter::{repeat, once};
impl<'a, N: 'a + fmt::Display, T: 'a + fmt::Display> fmt::Display for Derivation<'a, N, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        let &Derivation(ref tree) = self;

        for (pos, rule) in tree {
            if !pos.is_empty() {
                let pipes: Vec<String> = repeat("|  ".to_string())
                                            .take(pos.len())
                                            .chain(once("\n".to_string()))
                                            .chain(repeat("|  ".to_string()).take(pos.len() - 1))
                                            .chain(once("+- ".to_string()))
                                            .collect();
                buffer.push_str(&pipes.join(""));
            }
            buffer.push_str(format!("{}\n", rule).as_str());
        }
        write!(f, "{}", buffer)
    }
}

/// A mutliple context-free grammar.
#[derive(Clone, Debug)]
pub struct MCFG<N, T, W> {
    rules: Vec<PMCFGRule<N, T, W>>,
    initial: N,
}

impl<N, T> From<PMCFG<N, T, LogDomain<f64>>> for MCFG<N, T, LogDomain<f32>> {
    fn from(grammar: PMCFG<N, T, LogDomain<f64>>) -> Self {
        let PMCFG {
            rules, mut initial, ..
        } = grammar;
        assert!(initial.len() == 1);

        MCFG {
            rules: rules
                .into_iter()
                .map(|r| match r {
                    PMCFGRule {
                        head,
                        tail,
                        composition,
                        weight,
                    } => PMCFGRule {
                        head,
                        tail,
                        composition,
                        weight: LogDomain::new(weight.value() as f32).unwrap(),
                    },
                })
                .collect(),
            initial: initial.remove(0),
        }
    }
}


/// The index of a bracket in cs representation.
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

/// A cs representation of a grammar contains a generator automaton and a Dyck language.
#[derive(Debug, Serialize, Deserialize)]
pub struct CSRepresentation<N, T, F, S>
where
    N: Ord + Hash + Clone,
    T: Ord + Hash + Clone,
    F: FilterAutomaton<T> + Serialize,
    S: GeneratorStrategy<T>
{
    generator: S::Generator,
    rules: HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>,
    filter: F
}



use self::automata::GeneratorStrategy;

impl<N, T, F, S> CSRepresentation<N, T, F, S> 
where
    N: Ord + Hash + Clone,
    T: Ord + Hash + Clone + ::std::fmt::Debug,
    F: FilterAutomaton<T> + Serialize,
    S: GeneratorStrategy<T>
{
    pub fn new<M>(strategy: S, grammar: M) -> Self
    where
        M: Into<MCFG<N, T, LogDomain<f32>>>
    {
        let MCFG{ initial, rules } = grammar.into();
        let mut irules = HashIntegeriser::new();
        for rule in rules {
            irules.integerise(rule);
        }
        
        let gen = strategy.create_generator_automaton(&irules, initial);
        let fil = F::new(&irules, &gen);
        CSRepresentation {
            generator: gen,
            filter: fil,
            rules: irules
        }
    }

    /// Produces a `CSGenerator` for a Chomsky-Schützenberger characterization and a `word`.
    pub fn generate(&self, word: &[T], beam: usize) -> CSGenerator<T, N> {
        let g = self.generator.generate(self.filter.fsa(word, &self.generator), beam);
        CSGenerator {
            candidates: g,
            rules: &self.rules,
        }
    }
}

fn from_brackets<N, T>(
    rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>,
    word: Vec<Delta<T>>,
) -> Option<Derivation<N, T>>
where
    N: Hash + Eq + Clone,
    T: Hash + Eq + Clone,
{
    let mut tree = BTreeMap::new();
    let mut pos = Vec::new();

    for sigma in word {
        match sigma {
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

    Some(Derivation(
        tree.into_iter()
            .map(|(pos, i)| (pos, rules.find_value(i).unwrap()))
            .collect(),
    ))
}

/// Iterates Dyck words that represent a derivation for a word according to
/// the Chomsky-Schützenberger characterization of an MCFG.
pub struct CSGenerator<'a, T, N> 
where 
    T: 'a + PartialEq + Hash + Clone + Eq + Ord + fmt::Debug,
    N: 'a + Hash + Eq
{
    candidates: Box<Iterator<Item=Vec<BracketFragment<T>>> + 'a>,
    rules: &'a HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>,
}

impl<'a, N, T> Iterator for CSGenerator<'a, T, N>
where
    T: PartialEq + Hash + Clone + Eq + Ord + fmt::Debug,
    N: Hash + Eq + Clone + fmt::Debug,
{
    type Item = Derivation<'a, N, T>;

    fn next(&mut self) -> Option<Derivation<'a, N, T>> {
        let &mut CSGenerator {
            ref mut candidates,
            rules,
        } = self;

        for fragments in candidates {
            let candidate: Vec<Delta<T>> = BracketFragment::concat(fragments);
            if dyck::recognize(&candidate) {
                if let Some(derivation) = from_brackets(rules, candidate) {
                    return Some(derivation);
                }
            }
        }
       
        None
    }
}


#[cfg(test)]
mod test {

    #[test]
    fn csrep() {
        use VarT;
        use PMCFGRule;
        use Composition;
        use super::CSRepresentation;
        use super::LogDomain;
        use super::Derivation;
        use super::MCFG;
        use super::automata::{KellerGenerator, NaiveFilterAutomaton};

        let grammar = MCFG {
            initial: "S",
            rules: vec![
                PMCFGRule {
                    head: "S",
                    tail: vec!["S", "S"],
                    composition: Composition {
                        composition: vec![vec![VarT::Var(0, 0), VarT::Var(1, 0)]],
                    },
                    weight: LogDomain::new(0.3f32).unwrap(),
                },
                PMCFGRule {
                    head: "S",
                    tail: vec![],
                    composition: Composition {
                        composition: vec![vec![VarT::T('A')]],
                    },
                    weight: LogDomain::new(0.7f32).unwrap(),
                },
            ],
        };
        let d1 = Derivation(vec![(vec![], &grammar.rules[1])].into_iter().collect());
        let d2 = Derivation(
            vec![
                (vec![], &grammar.rules[0]),
                (vec![0], &grammar.rules[1]),
                (vec![1], &grammar.rules[1]),
            ].into_iter()
                .collect(),
        );

        let cs = CSRepresentation::<&str, char, NaiveFilterAutomaton<char>, KellerGenerator>::new(
            KellerGenerator,
            grammar.clone()
        );
        assert_eq!(
            cs.generate(&['A'], 10).next(),
            Some(d1)
        );
        assert_eq!(
            cs.generate(&['A', 'A'], 10).next(),
            Some(d2)
        );
    }
}
