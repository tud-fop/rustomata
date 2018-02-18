use integeriser::{HashIntegeriser, Integeriser};
use pmcfg::{PMCFGRule};
use std::hash::Hash;
use std::collections::{HashMap, BTreeSet, HashSet};
use std::rc::Rc;

use lcfrs::cs_representation::bracket_fragment::BracketFragment;
use super::{FilterAutomaton};
use lcfrs::cs_representation::automata::{StateTransition, StateInstruction, FiniteAutomaton, GeneratorAutomaton};

use lcfrs::cs_representation::rule_fragments::{fragments};
use util::{IntMap};
use recognisable::{Search};

use Transition;

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
    free_nts: Vec<usize>,
    free_brackets: Vec<usize>,

    nt: IntMap<Vec<(Vec<usize>, Vec<usize>, usize)>>,
    nt_and_t: HashMap<T, Vec<(Vec<usize>, Vec<(Vec<T>, usize)>, usize)>>
}

impl<'a, T> FilterAutomaton<'a, T> for InsideFilterAutomaton<T>
where
    T: Hash + Eq + Clone + 'a,
{
    fn new<N, W, I, R>(
        grammar_rules: R,
        integeriser: &I,
        reference: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> Self
    where
        N: Hash + Eq + Clone + 'a,
        W: Eq + Clone + 'a,
        I: Integeriser<Item=PMCFGRule<N, T, W>>,
        R: Iterator<Item=&'a PMCFGRule<N, T, W>>
    {
        let mut dependencies = HashIntegeriser::new();
        let bracket_integeriser = reference.get_integeriser();
        
        let mut free_nts = Vec::new();
        let mut free_brackets = Vec::new();
        
        let mut nt = IntMap::default();
        let mut nt_and_t = HashMap::new();

        for rule in grammar_rules {
            let frags: Vec<_> = fragments(rule).collect();
            let head = dependencies.integerise(rule.head.clone());

            let mut tss_brackets = Vec::new();
            for fragment in frags {
                let bw: usize = bracket_integeriser.find_key(&fragment.bracket_word(integeriser)).unwrap();
                let ts: Vec<T> = fragment.terminals().iter().map(|t| *t).cloned().collect();
                tss_brackets.push((ts, bw));
            }

            let deps: Vec<_> = rule.tail.iter().map(|nt| dependencies.integerise(nt.clone())).collect();
            
            if tss_brackets.iter().all(|&(ref ts, _)| ts.is_empty()) {
                if rule.tail.is_empty() {
                    free_nts.push(head);
                    free_brackets.extend(tss_brackets.into_iter().map(|(_, bw)| bw));
                } else {
                    let bws: Vec<_> = tss_brackets.into_iter().map(|(_, bw)| bw).collect();
                    for a in &deps {
                        nt.entry(*a).or_insert_with(Vec::new).push((deps.clone(), bws.clone(), head));
                    }
                }
            } else {
                let t = (*tss_brackets.iter().flat_map(| &(ref ts, _) | (*ts).iter()).next().unwrap()).clone();
                nt_and_t.entry(t).or_insert_with(Vec::new).push((deps.clone(), tss_brackets.clone(), head));
            }
        }
        
        InsideFilterAutomaton{ free_nts, free_brackets, nt, nt_and_t }
    }


    fn fsa(
        &self,
        word: &[T],
        reference: &GeneratorAutomaton<BracketFragment<T>>,
    ) -> FiniteAutomaton<BracketFragment<T>, ()> {

        let mut arcs: Vec<StateTransition<usize, usize, ()>> = Vec::new();
        for b in &self.free_brackets {
            arcs.extend((0..word.len()+1).map(|i| Transition{ instruction: StateInstruction(i, i), word: vec![*b], weight: () }));
        }
        let mut nts: BTreeSet<usize> = self.free_nts.iter().cloned().collect();

        let epsilons = self.nt.clone();
        let mut with_ts = IntMap::default();

        let symbols: HashSet<_> = word.iter().cloned().collect();
        for symbol in symbols {
            for &(ref required_nts, ref ts_b_s, n) in self.nt_and_t.get(&symbol).unwrap_or(&Vec::new()) {
                let pos_bs: Vec<Vec<_>> = ts_b_s.iter().map(
                    |&(ref terms, bs)| {
                        if terms.is_empty() {
                            (0..word.len()+1).map(|i| (i, bs, i)).collect()
                        } else {
                            word.windows(terms.len())
                                .enumerate()
                                .filter_map(| (i, window) | if window == terms.as_slice() { Some((i, bs, i+terms.len())) } else { None } )
                                .collect()
                        }
                    }
                ).collect();
                if pos_bs.iter().all(|matches| !matches.is_empty()){
                    if required_nts.is_empty() {
                        arcs.extend(
                            pos_bs.into_iter()
                                  .flat_map(|v| v.into_iter()
                                                 .map(
                                                    | (from, label, to) | 
                                                    Transition{ 
                                                        instruction: StateInstruction(from, to),
                                                        word: vec![label],
                                                        weight: ()
                                                    }
                                                 )
                                  )
                        );
                        nts.insert(n);
                    } else {
                        for nt in &nts {
                            with_ts.entry(*nt).or_insert_with(Vec::new).push((nts.clone(), pos_bs.clone(), n));
                        }
                    }
                }
            }
        }

        let initials: Vec<_> = nts.iter().cloned().collect();
        for _ in Search::unweighted(
            initials, 
            | a | {
                let mut succ = Vec::new();
                for &(ref requires, ref bs, ensures) in epsilons.get(&a).unwrap_or(&Vec::new()) {
                    if requires.iter().all(|i| nts.contains(i)) {
                        nts.insert(ensures);
                        for q in 0..word.len()+1 {
                            for b in bs {
                                arcs.push(Transition{ instruction: StateInstruction(q, q), word: vec![*b], weight: ()});
                            }
                        }
                        succ.push(ensures);
                    }
                }
                for &(ref requires, ref bs, ensures) in with_ts.get(&a).unwrap_or(&Vec::new()) {
                    if requires.iter().all(|i| nts.contains(i)) {
                        nts.insert(ensures);
                        arcs.extend(bs.iter().flat_map(|b| b.iter().map(|&(q,l,q_)| Transition{ instruction: StateInstruction(q, q_), word: vec![l], weight: () })));
                        succ.push(ensures);
                    }
                }
                succ
            }
        ).uniques() {}

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
    use lcfrs::Lcfrs;
    use log_domain::LogDomain;
    use integeriser::{HashIntegeriser, Integeriser};
    use lcfrs::cs_representation::automata::{FilterAutomaton, GeneratorAutomaton, GeneratorStrategy,
                                      PushDownAutomaton, PushDownGenerator, NaiveFilterAutomaton, InsideFilterAutomaton};
    use lcfrs::cs_representation::bracket_fragment::BracketFragment;
    use util::agenda::Capacity;

    #[test]
    fn naive() {
        let mut grammar_string = String::new();
        File::open("examples/german-1.gr")
            .unwrap()
            .read_to_string(&mut grammar_string)
            .expect("failed to read file");

        let lcfrs: Lcfrs<String, String, LogDomain<f64>> = grammar_string.parse().unwrap();
        let (lcfrs_rules, initial) = lcfrs.destruct();
        let mut rules = HashIntegeriser::new();
        for rule in lcfrs_rules {
            rules.integerise(rule);
        }
        let word: Vec<String> = "Mögen Puristen aller Musikbereiche auch die Nase rümpfen , die Zukunft der Musik liegt für viele junge Komponisten im Crossover-Stil .".split_whitespace().map(|s| s.to_string()).collect();

        let generator: PushDownAutomaton<BracketFragment<String>, LogDomain<f64>> =
            PushDownGenerator.create_generator_automaton(rules.values(), initial, &rules);
        let filter = InsideFilterAutomaton::new(rules.values().iter(), &rules, &generator);
        let naivefilter = NaiveFilterAutomaton::new(rules.values().iter(), &rules, &generator);
        // eprintln!("{:?}", filter);

        let filter_automaton = filter.fsa(word.as_slice(), &generator);

        eprintln!("{}", generator);
        eprintln!("{:?}", filter);
        for (from, arcs_from) in filter_automaton.arcs.iter().enumerate() {
            for (label, &(to, _)) in arcs_from {
                eprintln!("{} → {} {}", from, generator.get_integeriser().find_value(*label).unwrap(), to);
            }
        }
        eprintln!("{}", generator.clone().intersect(&filter_automaton));


        let naive_automaton = naivefilter.fsa(word.as_slice(), &generator);
        eprintln!("{}", generator.clone().intersect(&naive_automaton).arcs.iter().flat_map(|aw| aw.values()).count());
        
        let words: Option<Vec<BracketFragment<String>>> =
            GeneratorAutomaton::generate(
                GeneratorAutomaton::intersect(&generator, filter_automaton.clone()),
                Capacity::Infinite
            ).next();
        eprintln!("{:?}", words);
    }

}