use integeriser::{Integeriser};
use pmcfg::{PMCFGRule};
use std::hash::Hash;
use std::collections::HashMap;
use std::rc::Rc;

use lcfrs::cs_representation::bracket_fragment::{BracketFragment};
use super::{FilterAutomaton};
use lcfrs::cs_representation::automata::{FiniteArc, FiniteAutomaton, GeneratorAutomaton};

use lcfrs::cs_representation::rule_fragments::fragments;

/// The `NaiveFilterAutomaton` will produce a `FiniteAutomaton` that loops all
/// brackets fragments in Δ' that do not contain any terminal symbols.
/// For each word w ∈ Σ, it will produce an automaton with the set of states Q = { 0, …, |w| }
/// with a loop for each state and a transition i → [δᵢ] i+1 if h(δᵢ) = wᵢ.
/// (TODO: multiple symbols in one bracket fragment) 
/// 
/// The `NaiveFilterAutomaton` will store bracket fragments that contain terminals
/// and all bracket fragments that do not.
#[derive(Serialize, Deserialize, Debug)]
pub struct NaiveFilterAutomaton<T>
where
    T: Eq + Hash,
{
    brackets_with: HashMap<T, Vec<(Vec<T>, usize)>>,
    epsilon_brackets: Vec<usize>,
}

impl<'a, T> FilterAutomaton<'a, T> for NaiveFilterAutomaton<T>
where
    T: Hash + Eq + Clone + 'a,
{
    fn new<N: 'a, W: 'a, I, R>(
        grammar_rules: R,
        integeriser: &I,
        reference: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> Self
    where
        N: Hash + Eq + Clone,
        W: Eq + Clone,
        I: Integeriser<Item=PMCFGRule<N, T, W>>,
        R: Iterator<Item=&'a PMCFGRule<N, T, W>>
    {
        let integerizer = reference.get_integeriser();
        let mut brackets_with = HashMap::new();
        let mut epsilon_brackets = Vec::new();

        for fragment in grammar_rules.flat_map(|r| fragments(r)) {
            let brackets = integerizer.find_key(&fragment.bracket_word(integeriser)).unwrap();

            if fragment.terminals().is_empty() {
                epsilon_brackets.push(brackets);
            } else {
                for symbol in fragment.terminals() {
                    brackets_with.entry((*symbol).clone()).or_insert(Vec::new()).push(
                        (fragment.terminals().iter().map(|s| (*s).clone()).collect(), brackets)
                    );
                }
            }
        }

        NaiveFilterAutomaton{
            brackets_with,
            epsilon_brackets
        }

        // for rule_id in 0..(grammar.size()) {
        //     let rule = grammar.find_value(rule_id).unwrap();
        //     for (j, component) in rule.composition.composition.iter().enumerate() {
        //         let mut bracket_terminals =
        //             vec![Bracket::Open(BracketContent::Component(rule_id, j))];
        //         let mut terminals = Vec::new();
        //         for symbol in component {
        //             match *symbol {
        //                 VarT::T(ref t) => {
        //                     terminals.push(t.clone());
        //                     bracket_terminals
        //                         .push(Bracket::Open(BracketContent::Terminal(t.clone())));
        //                     bracket_terminals
        //                         .push(Bracket::Close(BracketContent::Terminal(t.clone())));
        //                 }
        //                 VarT::Var(i, j_) => {
        //                     bracket_terminals
        //                         .push(Bracket::Open(BracketContent::Variable(rule_id, i, j_)));

        //                     match vec_split(terminals) {
        //                         (Some(first), tail) => {
        //                             brackets_with.entry(first).or_insert_with(Vec::new).push(
        //                                 (
        //                                     tail,
        //                                     integerizer
        //                                         .find_key(&BracketFragment(bracket_terminals))
        //                                         .unwrap(),
        //                                 )
        //                             )
        //                         }
        //                         (None, _) => {
        //                             epsilon_brackets.push(
        //                                 integerizer
        //                                     .find_key(&BracketFragment(bracket_terminals))
        //                                     .unwrap(),
        //                             );
        //                         }
        //                     }

        //                     terminals = Vec::new();
        //                     bracket_terminals =
        //                         vec![Bracket::Close(BracketContent::Variable(rule_id, i, j_))];
        //                 }
        //             }
        //         }
        //         bracket_terminals.push(Bracket::Close(BracketContent::Component(rule_id, j)));
        //         match vec_split(terminals) {
        //             (Some(first), tail) => brackets_with.entry(first).or_insert_with(Vec::new).push(
        //                 (
        //                     tail,
        //                     integerizer
        //                         .find_key(&BracketFragment(bracket_terminals))
        //                         .unwrap(),
        //                 )
        //             ),
        //             (None, _) => epsilon_brackets.push(
        //                 integerizer
        //                     .find_key(&BracketFragment(bracket_terminals))
        //                     .unwrap(),
        //             ),
        //         }
        //     }
        // }

        // NaiveFilterAutomaton {
        //     brackets_with,
        //     epsilon_brackets,
        // }
    }


    fn fsa(
        &self,
        word: &[T],
        reference: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> FiniteAutomaton<BracketFragment<T>, ()> {
        let mut arcs = Vec::new();
        for i in 0..(word.len()) {
            for &(ref terminals, i_brackets) in self.brackets_with.get(&word[i]).unwrap_or(&Vec::new()) {
                if word[i..(i + terminals.len())] == terminals[..] {
                    arcs.push(FiniteArc{
                        from: i,
                        to: i + terminals.len(),
                        label: i_brackets,
                        weight: ()
                    });
                }
            }
            arcs.extend(self.epsilon_brackets.iter().map(
                |brackets|
                FiniteArc{ from: i, to: i, label: *brackets, weight: ()}
            ));
        }
        arcs.extend(self.epsilon_brackets.iter().map(
            |brackets|
            FiniteArc{ from: word.len(), to: word.len(), label: *brackets, weight: ()}
        ));
        FiniteAutomaton::from_integerized(
            arcs,
            0,
            vec![word.len()],
            Rc::clone(&reference.get_integeriser()),
        )
        // let mut arcs: Vec<FiniteArc<usize, usize, ()>> = Vec::new();
        // let mut exec_stack: Vec<(usize, &[T], usize, usize)> =
        //     get_brackets_with(word, &self.brackets_with)
        //         .into_iter()
        //         .map(|(w, bs, n)| (0, w, bs, n))
        //         .collect();
        // while !exec_stack.is_empty() {
        //     let (q0, remaining_word, brackets, q1) = exec_stack.remove(0);
        //     exec_stack.extend(
        //         get_brackets_with(remaining_word, &self.brackets_with)
        //             .into_iter()
        //             .map(|(w, bs, n)| (q1, w, bs, q1 + n)),
        //     );
        //     arcs.push(FiniteArc {
        //         from: q0,
        //         to: q1,
        //         label: brackets,
        //         weight: (),
        //     });
        // }
        // for q in 0..(word.len() + 1) {
        //     for brackets in &self.epsilon_brackets {
        //         arcs.push(FiniteArc {
        //             from: q,
        //             to: q,
        //             label: *brackets,
        //             weight: (),
        //         });
        //     }
        // }

        // FiniteAutomaton::from_integerized(
        //     arcs,
        //     0,
        //     vec![word.len()],
        //     Rc::clone(&reference.get_integeriser()),
        // )
    }
}

#[cfg(test)]
mod test {

    use std::fs::File;
    use std::io::Read;
    use lcfrs::Lcfrs;
    use log_domain::LogDomain;
    use integeriser::{HashIntegeriser, Integeriser};
    use lcfrs::cs_representation::automata::{FilterAutomaton, GeneratorAutomaton, GeneratorStrategy,
                                      PushDownAutomaton, PushDownGenerator, NaiveFilterAutomaton};
    use lcfrs::cs_representation::bracket_fragment::BracketFragment;
    use util::agenda::Capacity;

    #[test]
    fn naive() {
        let mut grammar_string = String::new();
        File::open("examples/example_mcfg.gr")
            .unwrap()
            .read_to_string(&mut grammar_string)
            .expect("failed to read file");

        let lcfrs: Lcfrs<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();
        let (lcfrs_rules, initial) = lcfrs.destruct();
        let mut rules = HashIntegeriser::new();
        for rule in lcfrs_rules {
            rules.integerise(rule);
        }
        let word: Vec<String> = "a e c".split_whitespace().map(|s| s.to_string()).collect();

        let generator: PushDownAutomaton<BracketFragment<String>, LogDomain<f64>> =
            PushDownGenerator.create_generator_automaton(&rules, initial);
        
        let filter = NaiveFilterAutomaton::new(rules.values().iter(), &rules, &generator);

        eprintln!("{:?}", filter);

        let filter_automaton = filter.fsa(word.as_slice(), &generator);
        let words: Option<Vec<BracketFragment<String>>> =
            GeneratorAutomaton::generate(
                GeneratorAutomaton::intersect(&generator, filter_automaton.clone()),
                Capacity::Infinite
            ).next();

        eprintln!("{}", generator.intersect(&filter_automaton));
        eprintln!("{:?}", words);
    }

}
