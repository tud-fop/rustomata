extern crate openfsa;

use std::hash::Hash;
use integeriser::{Integeriser, HashIntegeriser};
use std::collections::{HashSet};

use pmcfg::{PMCFG, PMCFGRule, VarT};
use dyck::multiple::{Bracket, MultipleDyckLanguage};
use self::openfsa::fsa::{Arc, Automaton};

/// The index of a bracket in cs representation.
/// Assumes integerized rules.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum BracketContent<T> {
    Terminal(T),
    Component(usize, usize),
    Variable(usize, usize, usize)
}

/// A cs representation of a grammar contains a generator automaton and a Dyck language.
#[derive(Debug)]
pub struct CSRepresentation<N: Eq + Hash, T: Eq + Hash + ::std::fmt::Debug, W: Eq + Hash> {
    generator: Automaton<Bracket<BracketContent<T>>>,
    dyck: MultipleDyckLanguage<BracketContent<T>>,
    rules: HashIntegeriser<PMCFGRule<N, T, W>>
}

fn terminal_brackets<T: PartialEq + Clone>(terminals: &[T]) -> Vec<Bracket<BracketContent<T>>> {
    let mut result = Vec::new();
    
    for t in terminals {
        result.push(Bracket::Open(BracketContent::Terminal(t.clone())));
        result.push(Bracket::Close(BracketContent::Terminal(t.clone())));
    }
    
    result
}

impl<N: Eq + Hash + Clone, T: Eq + Hash + Clone+ ::std::fmt::Debug, W: Eq + Hash + Clone> CSRepresentation<N, T, W> {
    pub fn new(grammar: PMCFG<N, T, W>) -> Self {
        assert!(grammar.initial.len() == 1);
        
        let mut rules = HashIntegeriser::new();
        let mut alphabet = HashSet::new();
        let mut partition: Vec<Vec<BracketContent<T>>> = Vec::new();
        let mut arcs = Vec::new();

        for rule in grammar.rules.into_iter() {
            let rule_id = rules.integerise(rule.clone());
            let mut variable_brackets: Vec<Vec<BracketContent<T>>> = vec![vec![];rule.tail.len()];

            for (component, composition) in rule.composition.composition.iter().enumerate() {
                let mut first = Bracket::Open(BracketContent::Component(rule_id, component));
                let mut from = Bracket::Open(rule.head.clone());
                let mut current_sequence = Vec::new();
                for symbol in composition {
                    match symbol {
                        &VarT::T(ref t) => {
                            current_sequence.push(t.clone());
                            alphabet.insert(t.clone());
                        },
                        &VarT::Var(ref i, ref j) => {
                            let mut bracketword = terminal_brackets(current_sequence.as_slice());
                            bracketword.insert(0, first);
                            bracketword.push(Bracket::Open(BracketContent::Variable(rule_id, *i, *j)));
                            
                            arcs.push(Arc::new(from, Bracket::Open(rule.tail[*i].clone()), bracketword, 1f32));
                            
                            from = Bracket::Close(rule.tail[*i].clone());
                            first = Bracket::Close(BracketContent::Variable(rule_id, *i, *j));

                            match variable_brackets.get_mut(*i) {
                                Some(cell) => { cell.push(BracketContent::Variable(rule_id, *i, *j)); },
                                _ => ()
                            }
                        }
                    }
                }
                let mut bracketword = terminal_brackets(current_sequence.as_slice());
                bracketword.insert(0, first);
                bracketword.push(Bracket::Close(BracketContent::Component(rule_id, component)));
                
                arcs.push(Arc::new(from, Bracket::Close(rule.head.clone()), bracketword, 1f32));
            }

            partition.extend(variable_brackets);
            
            // parition of component brackets
            let component_brackets = (0..rule.composition.composition.len()).map(| comp | BracketContent::Component(rule_id, comp));
            partition.push(component_brackets.collect());
        }

        // unary partitions for terminals
        for symbol in alphabet {
            partition.push(vec![BracketContent::Terminal(symbol)]);
        }

        let r = Automaton::from_arcs(Bracket::Open(grammar.initial[0].clone()), vec![Bracket::Close(grammar.initial[0].clone())], arcs);
        let md = MultipleDyckLanguage::new(partition.iter().flat_map(|c| c.iter()).map(|x| x.clone()).collect(), partition);

        CSRepresentation{ generator: r, dyck: md, rules }
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn csrep() {
        use VarT;
        use PMCFG;
        use PMCFGRule;
        use std::marker::PhantomData;
        use Composition;

        let grammar = PMCFG{ initial: vec!["S"]
                           , _dummy: PhantomData
                           , rules: vec![PMCFGRule{ head: "S"
                                                  , tail: vec![]
                                                  , composition: Composition{ composition: vec![vec![VarT::T('A')]] }
                                                  , weight: 1
                                                  }]
                           };
        
        println!("{:?}", super::CSRepresentation::new(grammar));
        assert!(false);
    }
}