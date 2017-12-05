pub mod cli;

use std::hash::Hash;
use std::fmt;
use integeriser::{Integeriser, HashIntegeriser};
use std::collections::{HashSet, BTreeMap, BTreeSet};
use num_traits::One;

use pmcfg::{PMCFG, PMCFGRule, VarT};
use dyck::multiple::{Bracket, MultipleDyckLanguage};
use openfsa::fsa::{Arc, Automaton, generator};
use log_domain::LogDomain;
use dyck;

use util::partition::Partition;

pub mod generator_automaton;
use cs_representation::generator_automaton::{GeneratorAutomaton};

use std::fmt::{Display, Formatter, Error};

/// A derivation tree of PMCFG rules.
#[derive(PartialEq, Debug)]
pub struct Derivation<'a, N: 'a, T: 'a>(BTreeMap<Vec<usize>, &'a PMCFGRule<N, T, LogDomain<f32>>>);

// impl<'a, N, T> Derivation<'a, N, T>
// where
//     N: 'a,
//     T: 'a
// {
//     pub fn weight(&self) -> LogDomain<f32> {
//         let mut dweight = LogDomain::one();
//         let &Derivation(ref map) = self;
        
//         for &&PMCFGRule{ weight, .. } in map.values() {
//             dweight = dweight * weight;
//         }
        
//         dweight
//     }
// } 

impl<'a, N: 'a + fmt::Display, T: 'a + fmt::Display> fmt::Display for Derivation<'a, N, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        let &Derivation(ref tree) = self;

        for (pos, rule) in tree {
            if !pos.is_empty() {
                for _ in 0..(pos.len() - 1) {
                    buffer.push('\t');
                }
                buffer.push_str(" ⮡ ");
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
        let PMCFG { rules, mut initial, .. } = grammar;
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
where T: Display
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            BracketContent::Terminal(ref t) => write!(f, "_{{{}}}", t),
            BracketContent::Component(rule_id, comp_id) => write!(f, "_{}^{}", rule_id, comp_id),
            BracketContent::Variable(rule_id, i, j) => write!(f, "_{{{},{}}}^{}", rule_id, i, j)
        }
    }
}

/// A cs representation of a grammar contains a generator automaton and a Dyck language.
#[derive(Debug, Serialize, Deserialize)]
pub struct CSRepresentation<N: Eq + Hash + Clone, T: Ord + Hash + Clone> {
    generator: Automaton<Bracket<BracketContent<T>>>,
    dyck: Partition<BracketContent<T>>,

    // rules are integerized in CSRepresentation::new()
    // TODO: maybe integerize each `BracketContent` ?
    rules: HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>,

    // saves all brackets σ with h(σ)(ε) ≠ 0
    epsilon_brackets: Vec<BracketContent<T>>,
}

fn get_partition<N, T, F>(rules: &HashIntegeriser<PMCFGRule<N, T, F>>) -> Vec<BTreeSet<BracketContent<T>>> 
where
    T: Clone + Hash + Eq + Ord,
    N: Clone + Hash + Eq + Ord,
    F: Clone
{
    let mut cells = Vec::new();
    let mut terminals = HashSet::new();
    
    for rule_id in 0..(rules.size()) {
        let rule = rules.find_value(rule_id).unwrap();
        cells.push(
            (0..(rule.composition.composition.len())).map(|j| BracketContent::Component(rule_id, j)).collect()
        );
        cells.extend(
            {
                let mut succs = vec![BTreeSet::new();rule.tail.len()];
                for symbol in rule.composition.composition.iter().flat_map(|x| (*x).iter()) {
                    match *symbol {
                        VarT::T(ref t) => {
                            if !terminals.contains(t) {
                                terminals.insert(t.clone());
                            }
                        },
                        VarT::Var(i, j) => {
                            succs[i].insert(BracketContent::Variable(rule_id, i, j));
                        }
                    }
                }
                succs.into_iter()
            }
        )
    }
    cells.extend(
        terminals.into_iter().map(|t| vec![BracketContent::Terminal(t)].into_iter().collect())
    );

    cells
}

fn epsilon_brackets<T>(cells: &[BTreeSet<BracketContent<T>>]) -> Vec<BracketContent<T>>
where
    T: Clone
{
    let mut bracks = Vec::new();
    for cell in cells {
        if cell.iter().any(| bc | if let BracketContent::Terminal(_) = *bc { false } else { true } ) {
            bracks.extend(cell.iter().cloned())
        }
    }
    bracks
}

impl<N, T> CSRepresentation<N, T>
where
    N: Eq + Hash + Clone + ::std::fmt::Debug,
    T: Ord + Eq + Hash + Clone + ::std::fmt::Debug
{
    ///
    pub fn new<S, G: Into<MCFG<N, T, LogDomain<f32>>>>(strat: S, grammar: G) -> Self
    where
        S: GeneratorAutomaton,
        N: Ord + Clone + Eq,
        T: Ord + Clone + Eq
    {
        let MCFG{ rules, initial } = grammar.into();
        let mut rulemap = HashIntegeriser::new();
        for rule in rules {
            rulemap.integerise(rule);
        }
        let cells = get_partition(&rulemap);
        let eps = epsilon_brackets(&cells);
        
        CSRepresentation{
            generator: strat.convert(&rulemap, initial),
            dyck: Partition::new(cells).unwrap(),
            rules: rulemap,
            epsilon_brackets: eps
        }
    }

    /// Produces a `CSGenerator` for a Chomsky-Schützenberger characterization and a `word`.
    pub fn generate(&self, word: Vec<T>, n: usize) -> CSGenerator<T, N> {
        // filter automaton
        let mut arcs = Vec::new();
        let fin = word.len();

        for epsilon_bracket in self.epsilon_brackets.iter().cloned() {
            arcs.push(Arc::new(
                0,
                0,
                vec![Bracket::Open(epsilon_bracket.clone())],
                LogDomain::one(),
            ).unwrap());
            arcs.push(Arc::new(
                0,
                0,
                vec![Bracket::Close(epsilon_bracket)],
                LogDomain::one(),
            ).unwrap());
        }
        for (i, sigma) in word.into_iter().enumerate() {
            arcs.push(Arc::new(
                i,
                i + 1,
                vec![
                    Bracket::Open(BracketContent::Terminal(sigma.clone())),
                    Bracket::Close(BracketContent::Terminal(sigma.clone())),
                ],
                LogDomain::one(),
            ).unwrap());
            for epsilon_bracket in self.epsilon_brackets.iter().cloned() {
                arcs.push(Arc::new(
                    i + 1,
                    i + 1,
                    vec![Bracket::Open(epsilon_bracket.clone())],
                    LogDomain::one(),
                ).unwrap());
                arcs.push(Arc::new(
                    i + 1,
                    i + 1,
                    vec![Bracket::Close(epsilon_bracket)],
                    LogDomain::one(),
                ).unwrap());
            }
        }

        let filter = self.generator.from_arcs_with_same_labels(
            0,
            vec![fin],
            arcs,
        );

        let &CSRepresentation {
            ref generator,
            ref dyck,
            ref rules,
            ..
        } = self;

        CSGenerator {
            candidates: generator.clone().intersect(&filter).generate(n),
            checker: MultipleDyckLanguage::new(dyck),
            rules,
        }
    }

    fn from_brackets(
        rules: &HashIntegeriser<PMCFGRule<N, T, LogDomain<f32>>>,
        word: Vec<Bracket<BracketContent<T>>>,
    ) -> Derivation<N, T> {
        let mut tree = BTreeMap::new();
        let mut pos = Vec::new();

        for sigma in word {
            match sigma {
                Bracket::Open(BracketContent::Component(rule_id, _)) => {
                    tree.insert(pos.clone(), rules.find_value(rule_id).unwrap());
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

        Derivation(tree)
    }
}

/// Iterates Dyck words that represent a derivation for a word according to
/// the Chomsky-Schützenberger characterization of an MCFG.
pub struct CSGenerator<'a, T: 'a + PartialEq + Hash + Clone + Eq + Ord + fmt::Debug, N: 'a + Hash + Eq> {
    candidates: generator::BatchGenerator<Bracket<BracketContent<T>>>,

    checker: MultipleDyckLanguage<BracketContent<T>>,
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
            ref checker,
            rules,
        } = self;

        let mut cans = 0;
        let mut dycks = 0;

        for candidate_batch in candidates {
            for (candidate, _) in candidate_batch {
                cans += 1;
                if dyck::recognize(&candidate){
                    dycks += 1;
                    if checker.recognize(&candidate) {
                        eprintln!("found after {} candidates, where {} were dyck words", cans, dycks);
                        return Some(CSRepresentation::from_brackets(rules, candidate));
                    }
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
        use cs_representation::generator_automaton::naive::{NaiveGeneratorAutomaton};

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
                    composition: Composition { composition: vec![vec![VarT::T('A')]] },
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

        assert_eq!(
            CSRepresentation::new(NaiveGeneratorAutomaton, grammar.clone())
                .generate(vec!['A'], 2usize)
                .next(),
            Some(d1)
        );
        assert_eq!(
            CSRepresentation::new(NaiveGeneratorAutomaton, grammar.clone())
                .generate(vec!['A', 'A'], 2usize)
                .next(),
            Some(d2)
        );
    }
}
