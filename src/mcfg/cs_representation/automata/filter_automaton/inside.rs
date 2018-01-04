use mcfg::cs_representation::BracketContent;
use integeriser::{HashIntegeriser, Integeriser};
use pmcfg::{PMCFGRule, VarT};
use std::hash::Hash;
use dyck::Bracket;
use std::collections::{HashMap, BTreeSet};
use std::rc::Rc;

use mcfg::cs_representation::bracket_fragment::BracketFragment;
use super::{FilterAutomaton, get_brackets_with, vec_split};
use mcfg::cs_representation::automata::{FiniteArc, FiniteAutomaton, GeneratorAutomaton};

/// In contrast to the `NaiveFilterAutomaton`, the `FiniteAutomaton`
/// that is produced using the `InsideFilterAutomaton` will not loop
/// all bracket fragemnts in each state.
/// It analyses the gramar rules and only uses those bracket fragments that
/// correspond to a grammar rule that is reachable with respect to the set of symbols
/// we want to parse.
#[derive(Serialize, Deserialize, Debug)]
pub struct InsideFilterAutomaton<T>
where
    T: Eq + Hash,
{
    // rule depends on list of nonterms, has some bracketfragements and implies a nonterm,
    // key is some nonterm it depends on
    epsilon_brackets: HashMap<usize, Vec<(Vec<usize>, Vec<usize>, usize)>>,

    // ε-rules without any dependencies
    free_brackets: Vec<usize>,

    // as usal
    term_brackets: HashMap<T, Vec<(Vec<T>, usize)>>,

    // each terminal implies the lhs nonterminal of each rule it occurs in
    term_implies: HashMap<T, Vec<usize>>,

    // free nonterminals (ε rules in grammar)
    free: Vec<usize>,
}

impl<T> FilterAutomaton<T> for InsideFilterAutomaton<T>
where
    T: Hash + Eq + Clone,
{
    fn new<N, W>(
        grammar: &HashIntegeriser<PMCFGRule<N, T, W>>,
        reference: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> Self
    where
        W: Eq + Clone,
        N: Hash + Clone + Eq
    {
        let mut dependencies = HashIntegeriser::new();
        let integerizer = reference.get_integeriser();

        let mut term_brackets = HashMap::new();
        let mut term_implies = HashMap::new();
        let mut epsilon_brackets = HashMap::new();
        let mut free = Vec::new();
        let mut free_brackets = Vec::new();

        for rule_id in 0..(grammar.size()) {
            let rule = grammar.find_value(rule_id).unwrap();

            if rule.composition
                .composition
                .iter()
                .all(|component| component.is_empty()) && rule.tail.is_empty()
            {
                free.push(dependencies.integerise(rule.head.clone()));
            }

            for (j, component) in rule.composition.composition.iter().enumerate() {
                let mut bracket_terminals =
                    vec![Bracket::Open(BracketContent::Component(rule_id, j))];
                let mut terminals = Vec::new();
                let mut epsilon_brackets_per_rule = Vec::new();

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

                            for terminal in terminals.clone() {
                                term_implies
                                    .entry(terminal)
                                    .or_insert_with(Vec::new)
                                    .push(dependencies.integerise(rule.head.clone()));
                            }

                            match vec_split(terminals) {
                                (Some(first), tail) => {
                                    term_brackets.entry(first).or_insert_with(Vec::new).push((
                                        tail,
                                        integerizer
                                            .find_key(&BracketFragment(bracket_terminals))
                                            .unwrap(),
                                    ));
                                }
                                (None, _) => {
                                    epsilon_brackets_per_rule.push(
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
                for terminal in terminals.clone() {
                    term_implies
                        .entry(terminal)
                        .or_insert_with(Vec::new)
                        .push(dependencies.integerise(rule.head.clone()));
                }

                bracket_terminals.push(Bracket::Close(BracketContent::Component(rule_id, j)));
                match vec_split(terminals) {
                    (Some(first), tail) => term_brackets.entry(first).or_insert_with(Vec::new).push((
                        tail,
                        integerizer
                            .find_key(&BracketFragment(bracket_terminals))
                            .unwrap(),
                    )),
                    (None, _) => epsilon_brackets_per_rule.push(
                        integerizer
                            .find_key(&BracketFragment(bracket_terminals))
                            .unwrap(),
                    ),
                }

                if rule.tail.is_empty() {
                    free_brackets.extend(epsilon_brackets_per_rule);
                } else {
                    for nonterminal in rule.tail.iter().cloned() {
                        epsilon_brackets.entry(dependencies.integerise(nonterminal.clone())).or_insert_with(Vec::new).push(
                            (
                                rule.tail.iter().cloned().map(|n| dependencies.integerise(n)).collect(),
                                epsilon_brackets_per_rule.clone(),
                                dependencies.integerise(rule.head.clone())
                            )
                        )
                    }
                }
            }
        }

        InsideFilterAutomaton {
            term_brackets,
            free_brackets,
            epsilon_brackets,
            term_implies,
            free
        }
    }


    fn fsa(
        &self,
        word: &[T],
        reference: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> FiniteAutomaton<BracketFragment<T>, ()> {
        let mut arcs: Vec<FiniteArc<usize, usize, ()>> = Vec::new();
        
        let mut exec_stack: Vec<(usize, &[T], usize, usize)> =
            get_brackets_with(word, &self.term_brackets)
                .into_iter()
                .map(|(w, bs, n)| (0, w, bs, n))
                .collect();
        
        while !exec_stack.is_empty() {
            let (q0, remaining_word, brackets, q1) = exec_stack.remove(0);
            exec_stack.extend(
                get_brackets_with(remaining_word, &self.term_brackets)
                    .into_iter()
                    .map(|(w, bs, n)| (q1, w, bs, q1 + n)),
            );
            arcs.push(FiniteArc {
                from: q0,
                to: q1,
                label: brackets,
                weight: (),
            });
        }

        let mut used_eps_brackets = Vec::new();
        let mut ntset = BTreeSet::new();
        let mut ntstack = self.free.clone();
        for symbol in word {
            if let Some(ns) = self.term_implies.get(symbol) {
                ntstack.extend(ns.iter().cloned());
            }
        }
        while let Some(nt) = ntstack.pop() {
            ntset.insert(nt);
            for &(ref depends, ref brackets, ref implies) in self.epsilon_brackets.get(&nt).unwrap_or(&Vec::new()) {
                if depends.iter().all(|n| ntset.contains(n)) {
                    used_eps_brackets.extend(brackets.iter().cloned());
                    if !ntset.contains(implies){
                        ntstack.push(implies.clone());
                    }
                }
            }
        }

        for q in 0..(word.len() + 1) {
            for brackets in used_eps_brackets.iter().chain(self.free_brackets.iter()) {
                arcs.push(FiniteArc {
                    from: q,
                    to: q,
                    label: *brackets,
                    weight: (),
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
    use mcfg::MCFG;
    use log_domain::LogDomain;
    use integeriser::{HashIntegeriser, Integeriser};
    use mcfg::cs_representation::automata::{FilterAutomaton, GeneratorAutomaton, GeneratorStrategy,
                                      KellerAutomaton, KellerGenerator, NaiveFilterAutomaton, InsideFilterAutomaton};
    use mcfg::cs_representation::bracket_fragment::BracketFragment;
    use util::agenda::Capacity;

    #[test]
    fn naive() {
        let mut grammar_string = String::new();
        File::open("examples/example_mcfg.gr")
            .unwrap()
            .read_to_string(&mut grammar_string)
            .expect("failed to read file");

        let pmcfg: PMCFG<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();
        let grammar: MCFG<String, String, LogDomain<f64>> = pmcfg.into();
        let initial = grammar.initial;
        let mut rules = HashIntegeriser::new();
        for rule in grammar.rules {
            rules.integerise(rule);
        }
        let word: Vec<String> = "a e c".split_whitespace().map(|s| s.to_string()).collect();

        let generator: KellerAutomaton<BracketFragment<String>, LogDomain<f64>> =
            KellerGenerator.create_generator_automaton(&rules, initial);
        let filter = InsideFilterAutomaton::new(&rules, &generator);
        let naivefilter = NaiveFilterAutomaton::new(&rules, &generator);
        // eprintln!("{:?}", filter);

        let filter_automaton = filter.fsa(word.as_slice(), &generator);
        let words: Option<Vec<BracketFragment<String>>> =
            GeneratorAutomaton::generate(&generator, filter_automaton.clone(), Capacity::Infinite).next();

        eprintln!("{}", generator);
        eprintln!("{:?}", filter);
        eprintln!("{:?}", filter_automaton);
        eprintln!("{:?}", generator.clone().intersect(filter_automaton).arcs);


        let naive_automaton = naivefilter.fsa(word.as_slice(), &generator);
        eprintln!("{}", generator.intersect(naive_automaton).arcs.iter().flat_map(|aw| aw.values()).count());
        eprintln!("{:?}", words);
    }

}