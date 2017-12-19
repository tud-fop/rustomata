use cs_representation::BracketContent;
use integeriser::{HashIntegeriser, Integeriser};
use pmcfg::{PMCFGRule, VarT};
use std::hash::Hash;
use dyck::Bracket;
use log_domain::LogDomain;
use num_traits::One;
use std::collections::HashMap;
use std::rc::Rc;

use cs_representation::bracket_fragment::BracketFragment;
use super::FilterAutomaton;
use cs_representation::automata::{FiniteArc, FiniteAutomaton, GeneratorAutomaton};

#[derive(Serialize, Deserialize, Debug)]
pub struct NaiveFilterAutomaton<T>
where
    T: Eq + Hash,
{
    brackets_with: HashMap<T, Vec<(Vec<T>, usize)>>,
    epsilon_brackets: Vec<usize>,
}

fn vec_split<T>(mut xs: Vec<T>) -> (Option<T>, Vec<T>) {
    if xs.is_empty() {
        (None, xs)
    } else {
        let x = xs.remove(0);
        (Some(x), xs)
    }
}

fn get_brackets_with<'a, 'b, T>(
    current_word: &'a [T],
    brackets_with: &'b HashMap<T, Vec<(Vec<T>, usize)>>,
) -> Vec<(&'a [T], usize, usize)>
where
    T: Hash + Eq,
{
    if current_word.is_empty() || !brackets_with.contains_key(&current_word[0]) {
        Vec::new()
    } else {
        let mut results = Vec::new();
        for &(ref following_terminals, brackets) in brackets_with.get(&current_word[0]).unwrap() {
            if following_terminals.is_empty()
                || following_terminals.as_slice() == &current_word[1..(following_terminals.len())]
            {
                results.push((
                    &current_word[(following_terminals.len() + 1)..],
                    brackets,
                    following_terminals.len() + 1,
                ));
            }
        }
        results
    }
}

impl<T> FilterAutomaton<T> for NaiveFilterAutomaton<T>
where
    T: Hash + Eq + Clone,
{
    fn new<N, W>(
        grammar: &HashIntegeriser<PMCFGRule<N, T, W>>,
        reference: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> Self
    where
        N: Hash + Eq + Clone,
        W: Eq + Clone,
    {
        let integerizer = reference.get_integeriser();
        let mut brackets_with = HashMap::new();
        let mut epsilon_brackets = Vec::new();

        for rule_id in 0..(grammar.size()) {
            let rule = grammar.find_value(rule_id).unwrap();
            for (j, component) in rule.composition.composition.iter().enumerate() {
                let mut bracket_terminals =
                    vec![Bracket::Open(BracketContent::Component(rule_id, j))];
                let mut terminals = Vec::new();
                for symbol in component {
                    match *symbol {
                        VarT::T(ref t) => {
                            terminals.push(t.clone());
                            bracket_terminals
                                .push(Bracket::Open(BracketContent::Terminal(t.clone())));
                            bracket_terminals
                                .push(Bracket::Close(BracketContent::Terminal(t.clone())));
                        }
                        VarT::Var(i, j_) => {
                            bracket_terminals
                                .push(Bracket::Open(BracketContent::Variable(rule_id, i, j_)));

                            match vec_split(terminals) {
                                (Some(first), tail) => {
                                    brackets_with.entry(first).or_insert(Vec::new()).push((
                                        tail,
                                        integerizer
                                            .find_key(&BracketFragment(bracket_terminals))
                                            .unwrap(),
                                    ))
                                }
                                (None, _) => {
                                    epsilon_brackets.push(
                                        integerizer
                                            .find_key(&BracketFragment(bracket_terminals))
                                            .unwrap(),
                                    );
                                }
                            }

                            terminals = Vec::new();
                            bracket_terminals =
                                vec![Bracket::Close(BracketContent::Variable(rule_id, i, j_))];
                        }
                    }
                }
                bracket_terminals.push(Bracket::Close(BracketContent::Component(rule_id, j)));
                match vec_split(terminals) {
                    (Some(first), tail) => brackets_with.entry(first).or_insert(Vec::new()).push((
                        tail,
                        integerizer
                            .find_key(&BracketFragment(bracket_terminals))
                            .unwrap(),
                    )),
                    (None, _) => epsilon_brackets.push(
                        integerizer
                            .find_key(&BracketFragment(bracket_terminals))
                            .unwrap(),
                    ),
                }
            }
        }

        NaiveFilterAutomaton {
            brackets_with,
            epsilon_brackets,
        }
    }


    fn fsa(
        &self,
        word: &[T],
        reference: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> FiniteAutomaton<BracketFragment<T>> {
        let mut arcs: Vec<FiniteArc<usize, usize>> = Vec::new();
        let mut exec_stack: Vec<(usize, &[T], usize, usize)> =
            get_brackets_with(word, &self.brackets_with)
                .into_iter()
                .map(|(w, bs, n)| (0, w, bs, n))
                .collect();
        while !exec_stack.is_empty() {
            let (q0, remaining_word, brackets, q1) = exec_stack.remove(0);
            exec_stack.extend(
                get_brackets_with(remaining_word, &self.brackets_with)
                    .into_iter()
                    .map(|(w, bs, n)| (q1, w, bs, q1 + n)),
            );
            arcs.push(FiniteArc {
                from: q0,
                to: q1,
                label: brackets.clone(),
                weight: LogDomain::one(),
            });
        }
        for q in 0..(word.len() + 1) {
            for brackets in &self.epsilon_brackets {
                arcs.push(FiniteArc {
                    from: q,
                    to: q,
                    label: brackets.clone(),
                    weight: LogDomain::one(),
                });
            }
        }

        FiniteAutomaton::from_integerized(
            arcs,
            0,
            vec![word.len()],
            Rc::clone(&reference.get_integeriser()),
        )
    }
}

#[cfg(test)]
mod test {

    use std::fs::File;
    use std::io::Read;
    use pmcfg::PMCFG;
    use cs_representation::MCFG;
    use log_domain::LogDomain;
    use integeriser::{HashIntegeriser, Integeriser};
    use cs_representation::automata::{FilterAutomaton, GeneratorAutomaton, GeneratorStrategy,
                                      KellerAutomaton, KellerGenerator, NaiveFilterAutomaton};
    use cs_representation::bracket_fragment::BracketFragment;
    #[test]
    fn naive() {
        let mut grammar_string = String::new();
        File::open("examples/example_mcfg.gr")
            .unwrap()
            .read_to_string(&mut grammar_string)
            .expect("failed to read file");

        let pmcfg: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();
        let grammar: MCFG<String, String, LogDomain<f32>> = pmcfg.into();
        let initial = grammar.initial;
        let mut rules = HashIntegeriser::new();
        for rule in grammar.rules {
            rules.integerise(rule);
        }
        let word: Vec<String> = vec!["a".to_string(), "e".to_string(), "c".to_string()];

        let generator: KellerAutomaton<BracketFragment<String>> =
            KellerGenerator.create_generator_automaton(&rules, initial);
        let filter = NaiveFilterAutomaton::new(&rules, &generator);

        eprintln!("{:?}", filter);

        let filter_automaton = filter.fsa(word.as_slice(), &generator);
        let words: Vec<Vec<BracketFragment<String>>> =
            GeneratorAutomaton::generate(&generator, filter_automaton, 200).collect();

        eprintln!("{:?}", words);
    }

}
