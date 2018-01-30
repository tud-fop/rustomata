pub mod cli;

use serde::Serialize;
use util::agenda::Capacity;
use std::hash::Hash;
use std::fmt;
use integeriser::{HashIntegeriser, Integeriser};
use std::collections::BTreeMap;

use pmcfg::PMCFGRule;
use log_domain::LogDomain;
use dyck;

pub mod automata;
pub mod bracket_fragment;

use self::automata::*;
use self::bracket_fragment::BracketFragment;
use super::MCFG;

use std::fmt::{Display, Error, Formatter};
use time::PreciseTime;

/// A derivation tree of PMCFG rules.
#[derive(PartialEq, Debug)]
pub struct Derivation<'a, N: 'a, T: 'a>(
    BTreeMap<Vec<usize>, &'a PMCFGRule<N, T, LogDomain<f64>>>,
);

use std::iter::{once, repeat};
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

// impl Derivation<'a, String, String> {
//     fn printexport(&self, position: Vec<usize>, component: usize, parent: usize) {

//     }
//     pub fn export(&self) {
//         let stack = vec![(Vec::new(), 0, 500usize)]; // start with root pos
        
//         while let Some((position, component, parent)) = stack.pop() {
//             for symbol in self.0.get(position).unwrap().composition.composition[component] {
//                 match
//             }
//         }
//     }
// }

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
    F: for <'a> FilterAutomaton<'a, T> + Serialize,
    S: GeneratorStrategy<T>,
{
    generator: S::Generator,
    rules: HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>,
    filter: F,
}



use self::automata::GeneratorStrategy;

impl<N, T, F, S> CSRepresentation<N, T, F, S>
where
    N: Ord + Hash + Clone,
    T: Ord + Hash + Clone,
    F: for <'a> FilterAutomaton<'a, T> + Serialize,
    S: GeneratorStrategy<T>,
{
    /// Instantiates a CS representation for some `Into<MCFG>` and `GeneratorStrategy`.
    pub fn new<M>(strategy: S, grammar: M) -> Self
    where
        M: Into<MCFG<N, T, LogDomain<f64>>>,
    {
        let MCFG { initial, rules } = grammar.into();
        let mut irules = HashIntegeriser::new();
        for rule in rules {
            irules.integerise(rule);
        }

        let gen = strategy.create_generator_automaton(&irules, initial);
        let fil = F::new(irules.values().iter(), &irules, &gen);
        CSRepresentation {
            generator: gen,
            filter: fil,
            rules: irules,
        }
    }

    /// Produces a `CSGenerator` for a Chomsky-Schützenberger characterization and a `word`.
    pub fn generate(&self, word: &[T], beam: Capacity) -> CSGenerator<T, N> {
        let f = self.filter.fsa(word, &self.generator);
        let g = self.generator.intersect(f).generate(beam);
        
        CSGenerator {
            candidates: g,
            rules: &self.rules,
        }
    }

    pub fn debug(&self, word: &[T], beam: Capacity) {
        let time = PreciseTime::now();
        let f = self.filter.fsa(word, &self.generator);
        let filter_const = time.to(PreciseTime::now()).num_milliseconds();
        
        let filter_size = f.arcs.iter().flat_map(|map| map.values()).count();

        let time = PreciseTime::now();
        let g_ = self.generator.intersect(f);
        let intersection_time = time.to(PreciseTime::now()).num_milliseconds();

        let intersection_size = g_.size();
        
        eprint!(
            "{} {} {} {} {} {} {}", 
            self.rules.size(),
            word.len(),
            filter_const,
            filter_size,
            self.generator.size(),
            intersection_time,
            intersection_size
        );

        // time for generation
        let time = PreciseTime::now();
        let (cans, ptime) = match g_.generate(beam)
                .enumerate()
                .map(|(i, frag)| (i, BracketFragment::concat(frag)))
                .filter(| &(_, ref candidate) | dyck::recognize(candidate))
                .filter_map(|(i, candidate)| from_brackets(&self.rules, candidate).map(| _ | (i + 1)))
                .next() {
            
            Some(i) => (i, time.to(PreciseTime::now()).num_milliseconds()),
            None => (0, time.to(PreciseTime::now()).num_milliseconds())

        };

        eprintln!(
            " {} {}",
            cans,
            ptime
        );
    }
}

/// Reads off the `Derivation` tree from a generated bracket word.
fn from_brackets<N, T>(
    rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>,
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
    T: 'a + PartialEq + Hash + Clone + Eq + Ord,
    N: 'a + Hash + Eq,
{
    candidates: Box<Iterator<Item = Vec<BracketFragment<T>>> + 'a>,
    rules: &'a HashIntegeriser<PMCFGRule<N, T, LogDomain<f64>>>,
}

impl<'a, N, T> Iterator for CSGenerator<'a, T, N>
where
    T: PartialEq + Hash + Clone + Eq + Ord,
    N: Hash + Eq + Clone,
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
        use super::Capacity;
        use super::CSRepresentation;
        use super::LogDomain;
        use super::Derivation;
        use super::MCFG;
        use super::automata::{PushDownGenerator, NaiveFilterAutomaton};

        let grammar = MCFG {
            initial: "S",
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

        let cs = CSRepresentation::<&str, char, NaiveFilterAutomaton<char>, PushDownGenerator>::new(
            PushDownGenerator,
            grammar.clone(),
        );
        assert_eq!(cs.generate(&['A'], Capacity::Infinite).next(), Some(d1));
        assert_eq!(
            cs.generate(&['A', 'A'], Capacity::Infinite).next(),
            Some(d2)
        );
    }
}