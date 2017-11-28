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

/// A derivation tree of PMCFG rules.
#[derive(PartialEq, Debug)]
pub struct Derivation<'a, N: 'a, T: 'a>(BTreeMap<Vec<usize>, &'a PMCFGRule<N, T, LogDomain<f32>>>);

impl<'a, N: 'a, T: 'a> Derivation<'a, N, T> {
    pub fn weight(&self) -> LogDomain<f32> {
        let mut dweight = LogDomain::one();
        let &Derivation(ref map) = self;
        
        for &&PMCFGRule{ weight, .. } in map.values() {
            dweight = dweight * weight;
        }
        
        dweight
    }
} 

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

// implements ~(σ₁…σₙ) = ⟨σ₁⟩σ₁…⟨σₙ⟩σₙ
fn terminal_brackets<T: PartialEq + Clone>(terminals: &[T]) -> Vec<Bracket<BracketContent<T>>> {
    let mut result = Vec::new();

    for t in terminals {
        result.push(Bracket::Open(BracketContent::Terminal(t.clone())));
        result.push(Bracket::Close(BracketContent::Terminal(t.clone())));
    }

    result
}

impl<N: Eq + Hash + Clone + ::std::fmt::Debug, T: Ord + Eq + Hash + Clone + ::std::fmt::Debug>
    CSRepresentation<N, T> {
    ///
    pub fn new<G: Into<MCFG<N, T, LogDomain<f32>>>>(grammar: G) -> Self {
        let mcfg = grammar.into();

        let mut rules = HashIntegeriser::new();
        let mut alphabet = HashSet::new();
        let mut partition: Vec<BTreeSet<BracketContent<T>>> = Vec::new();
        let mut arcs = Vec::new();
        let mut epsilon_brackets = Vec::new();

        for rule in mcfg.rules {
            let comp_prob = rule.weight.pow(
                1f32 / rule.composition.composition.len() as f32,
            );
            let rule_id = rules.integerise(rule.clone());
            let mut variable_brackets: Vec<BTreeSet<BracketContent<T>>> =
                vec![BTreeSet::new(); rule.tail.len()];

            for (component, composition) in rule.composition.composition.iter().enumerate() {
                let rule_prob = comp_prob.pow( 
                    1f32 / ((composition.iter().filter(|x| x.is_var()).count() + 1) as f32)
                );
                let mut first = Bracket::Open(BracketContent::Component(rule_id, component));
                let mut from = Bracket::Open((rule.head.clone(), component));
                let mut current_sequence = Vec::new();
                for symbol in composition {
                    match *symbol {
                        VarT::T(ref t) => {
                            current_sequence.push(t.clone());
                            alphabet.insert(t.clone());
                        }
                        VarT::Var(ref i, ref j) => {
                            let mut bracketword = terminal_brackets(current_sequence.as_slice());
                            bracketword.insert(0, first);
                            bracketword.push(Bracket::Open(
                                BracketContent::Variable(rule_id, *i, *j),
                            ));
                            current_sequence = Vec::new();

                            arcs.push(Arc::new(
                                from,
                                Bracket::Open((rule.tail[*i].clone(), *j)),
                                bracketword,
                                rule_prob,
                            ));

                            from = Bracket::Close((rule.tail[*i].clone(), *j));
                            first = Bracket::Close(BracketContent::Variable(rule_id, *i, *j));

                            if let Some(cell) = variable_brackets.get_mut(*i) {
                                    cell.insert(BracketContent::Variable(rule_id, *i, *j));
                            }
                        }
                    }
                }
                let mut bracketword = terminal_brackets(current_sequence.as_slice());
                bracketword.insert(0, first);
                bracketword.push(Bracket::Close(
                    BracketContent::Component(rule_id, component),
                ));

                arcs.push(Arc::new(
                    from,
                    Bracket::Close((rule.head.clone(), component)),
                    bracketword,
                    rule_prob,
                ));
            }

            partition.extend(variable_brackets.clone().into_iter());
            epsilon_brackets.extend(variable_brackets.iter().flat_map(|x| x.iter()).cloned());

            // parition of component brackets
            let component_brackets: BTreeSet<BracketContent<T>> =
                (0..rule.composition.composition.len())
                    .map(|comp| BracketContent::Component(rule_id, comp))
                    .collect();
            partition.push(component_brackets.clone());
            epsilon_brackets.extend(component_brackets.into_iter());
        }

        // unary partitions for terminals
        for symbol in alphabet {
            partition.push(vec![BracketContent::Terminal(symbol)].into_iter().collect());
        }
        let r = Automaton::from_arcs(
            &Bracket::Open((mcfg.initial.clone(), 0)),
            &[Bracket::Close((mcfg.initial.clone(), 0))],
            arcs.as_slice(),
        );
        let md = Partition::new(partition).unwrap();

        CSRepresentation {
            generator: r,
            dyck: md,
            rules,
            epsilon_brackets,
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
            ));
            arcs.push(Arc::new(
                0,
                0,
                vec![Bracket::Close(epsilon_bracket)],
                LogDomain::one(),
            ));
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
            ));
            for epsilon_bracket in self.epsilon_brackets.iter().cloned() {
                arcs.push(Arc::new(
                    i + 1,
                    i + 1,
                    vec![Bracket::Open(epsilon_bracket.clone())],
                    LogDomain::one(),
                ));
                arcs.push(Arc::new(
                    i + 1,
                    i + 1,
                    vec![Bracket::Close(epsilon_bracket)],
                    LogDomain::one(),
                ));
            }
        }

        let filter = self.generator.from_arcs_with_same_labels(
            &0,
            &[fin],
            arcs.as_slice(),
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

impl<
    'a,
    T: PartialEq + Hash + Clone + Eq + Ord + fmt::Debug,
    N: Hash + Eq + Clone + fmt::Debug,
> Iterator for CSGenerator<'a, T, N> {
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
            for (candidate, weight) in candidate_batch {
                cans += 1;
                if dyck::recognize(&candidate){
                    dycks += 1;
                    if checker.recognize(&candidate) {
                        eprintln!("found after {} candidates, where {} were dyck words", cans, dyck);
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
            CSRepresentation::new(grammar.clone())
                .generate(vec!['A'], 2usize)
                .next(),
            Some(d1)
        );
        assert_eq!(
            CSRepresentation::new(grammar.clone())
                .generate(vec!['A', 'A'], 2usize)
                .next(),
            Some(d2)
        );
    }
}
