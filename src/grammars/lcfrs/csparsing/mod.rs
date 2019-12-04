mod automaton;
mod cowderiv;

use super::Lcfrs;

use crate::dyck::Bracket;
use crate::grammars::pmcfg::PMCFGRule;
use crate::util::{factorizable::Factorizable, tree::GornTree};
use num_traits::{One, Zero};
use std::time::{Duration, Instant};
use std::{
    collections::BTreeMap,
    fmt::{Display, Error, Formatter},
    hash::Hash,
    ops::Mul,
};

use self::automaton::{Automaton, RuleMaskBuilder, SxOutside};

/// The indices of a bracket in a CS representation for an lcfrs.
/// Assumes integerized an itergerized set of (at most 2^32) rules and fanouts
/// and arities ≤ 2^8.
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, PartialOrd, Ord, Serialize, Deserialize)]
pub enum BracketContent {
    /// We construe `Ignore` as a parenthesis without index; it is introduced
    /// for binarization.
    Ignore,
    /// We do not store the specific terminal as bracket index.
    Terminal,
    Component(u32, u8),
    Variable(u32, u8, u8),
}

impl BracketContent {
    #[inline(always)]
    pub fn is_ignore(self) -> bool {
        match self {
            BracketContent::Ignore => true,
            _ => false,
        }
    }
}

type Delta = Bracket<BracketContent>;

impl Display for BracketContent {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            BracketContent::Component(rule_id, comp_id) => write!(f, "_{}^{}", rule_id, comp_id),
            BracketContent::Variable(rule_id, i, j) => write!(f, "_{{{},{}}}^{}", rule_id, i, j),
            BracketContent::Terminal => write!(f, "_{{TERM}}"),
            _ => Ok(()),
        }
    }
}

/// A C-S representation of a grammar.
#[derive(Debug, Serialize, Deserialize)]
pub struct CSRepresentation<N, T, W>
where
    T: Eq + Hash,
{
    generator: Automaton<T, W>,
    estimates: SxOutside<W>,
    rulemaskbuilder: RuleMaskBuilder<T>,
    rules: Vec<PMCFGRule<N, T, W>>,
}

pub struct GeneratorBuilder<'a, N, T: Eq + Hash, W> {
    grammar: &'a CSRepresentation<N, T, W>,
    candidates: Option<usize>,
    beam: Option<usize>,
    delta: W,
    root_prediction: bool,
}

impl<'a, N, T, W> GeneratorBuilder<'a, N, T, W>
where
    T: Eq + Hash + Clone,
    W: Zero + Ord + Copy + One + Mul<Output = W>,
    N: Clone,
{
    pub fn set_candidates(&mut self, c: usize) {
        self.candidates = Some(c);
    }
    pub fn set_beam(&mut self, b: usize) {
        self.beam = Some(b);
    }
    pub fn set_delta(&mut self, d: W) {
        self.delta = d;
    }
    pub fn allow_root_prediction(&mut self) {
        self.root_prediction = true;
    }

    pub fn with_fallback(
        &self,
        word: &[T],
    ) -> (
        impl Iterator<Item = GornTree<&'a PMCFGRule<N, T, W>>> + 'a,
        Option<GornTree<PMCFGRule<N, T, W>>>,
    ) {
        let &Self {
            grammar,
            mut candidates,
            beam,
            delta,
            ..
        } = self;
        let realbeam = beam.unwrap_or_else(|| grammar.generator.states());
        let rulemask = grammar.rulemaskbuilder.build(word);
        let mut word_iterator = grammar
            .generator
            .generate(word, realbeam, delta, &grammar.estimates, rulemask)
            .peekable();
        let first = word_iterator
            .peek()
            .map(|w| cowderiv::CowDerivation::new(w).fallback(&grammar.rules));

        let count_candidates = move |_: &Vec<Delta>| -> bool {
            candidates.as_mut().map_or(true, |c| {
                if *c == 0 {
                    false
                } else {
                    *c -= 1;
                    true
                }
            })
        };

        (
            word_iterator
                .take_while(count_candidates)
                .filter_map(move |bs| grammar.toderiv(&bs)),
            first,
        )
    }

    pub fn debug(&self, word: &[T]) -> (usize, usize, Duration, DebugResult<N, T, W>) {
        let starting_time = Instant::now();
        let &Self {
            grammar,
            mut candidates,
            beam,
            delta,
            ..
        } = self;
        let rulemask = grammar.rulemaskbuilder.build(word);
        let realbeam = beam.unwrap_or_else(|| grammar.generator.states());
        let count_candidates = move |_: &Vec<Delta>| -> bool {
            candidates.as_mut().map_or(true, |c| {
                if *c == 0 {
                    false
                } else {
                    *c -= 1;
                    true
                }
            })
        };
        let word_iterator =
            grammar
                .generator
                .generate(word, realbeam, delta, &grammar.estimates, rulemask);
        let mut word_iterator = word_iterator.take_while(count_candidates).peekable();

        let mut enumerated_words = 0;

        let o_fallback_word = word_iterator.peek().cloned();
        let o_parse_tree = word_iterator
            .filter_map(|cfg_deriv| {
                enumerated_words += 1;
                grammar.toderiv(&cfg_deriv)
            })
            .next();

        let debug_result = match (o_parse_tree, o_fallback_word) {
            (Some(t), _) => DebugResult::Parse(t.cloned(), enumerated_words),
            (None, Some(w)) => {
                let tree = cowderiv::CowDerivation::new(&w).fallback(&grammar.rules);
                DebugResult::Fallback(tree, enumerated_words)
            }
            (None, None) => DebugResult::Noparse,
        };

        (
            grammar.rules.len(),
            word.len(),
            starting_time.elapsed(),
            debug_result,
        )
    }
}

pub enum DebugResult<N, T, W> {
    Parse(GornTree<PMCFGRule<N, T, W>>, usize),
    Fallback(GornTree<PMCFGRule<N, T, W>>, usize),
    Noparse,
}

impl<N, T, W> CSRepresentation<N, T, W>
where
    T: Ord + Hash + Clone,
    W: Ord + Copy + One + Mul<Output = W>,
    N: Clone,
{
    /// Instantiates a CS representation for an `LCFRS`.
    pub fn new<M>(grammar: M, estimates_max_width: usize) -> Self
    where
        M: Into<Lcfrs<N, T, W>>,
        W: Factorizable + Zero,
        N: Hash + Eq,
    {
        assert!(estimates_max_width <= u8::max_value() as usize, "the maximum width for estimates should be less than {}", u8::max_value());
        let grammar = grammar.into();
        assert!(grammar.in_normal_form(), "the given grammar is not in normal form (ε-free and terminal-separated)");
        
        let (rules, initial) = grammar.destruct();
        let generator = {
            let rules_with_id = rules.iter().enumerate().map(|(i, r)| (i as u32, r));
            Automaton::from_grammar(rules_with_id, initial.clone())
        };
        let rulemaskbuilder = RuleMaskBuilder::new(rules.iter(), &initial);
        let estimates = SxOutside::from_automaton(&generator, estimates_max_width as u8);

        CSRepresentation {
            generator,
            rulemaskbuilder,
            estimates,
            rules,
        }
    }

    pub fn build_generator(&self) -> GeneratorBuilder<N, T, W>
    where
        W: Zero,
    {
        GeneratorBuilder {
            grammar: self,
            beam: None,
            delta: W::zero(),
            candidates: None,
            root_prediction: false,
        }
    }
}

impl<N, T, W> CSRepresentation<N, T, W>
where
    T: Hash + Eq,
{
    /// Reads off a parse tree from a multiply Dyck word. Fails if the word is not in R ∩ D.
    fn toderiv<'a>(&'a self, word: &[Delta]) -> Option<GornTree<&'a PMCFGRule<N, T, W>>> {
        let mut tree = BTreeMap::new();
        let mut pos = Vec::new();

        for sigma in word {
            match *sigma {
                Bracket::Open(BracketContent::Component(rule_id, _)) => {
                    let rule_at_pos = tree.entry(pos.clone()).or_insert(rule_id);
                    if rule_at_pos != &rule_id {
                        return None;
                    }
                }
                Bracket::Open(BracketContent::Variable(_, i, _)) => {
                    pos.push(i as usize);
                }
                Bracket::Close(BracketContent::Variable(_, _, _)) => {
                    pos.pop();
                }
                _ => (),
            }
        }

        Some(
            tree.into_iter()
                .map(|(pos, i)| (pos, &self.rules[i as usize]))
                .collect(),
        )
    }
}

#[cfg(test)]
mod test {
    use super::{CSRepresentation, Lcfrs};
    use crate::grammars::pmcfg::{Composition, PMCFGRule, VarT};
    use log_domain::LogDomain;

    #[test]
    fn csrep() {
        let grammar = lcfrs();
        let d1 = vec![(vec![], &grammar.rules[1])].into_iter().collect();
        let d2 = vec![
            (vec![], &grammar.rules[0]),
            (vec![0], &grammar.rules[1]),
            (vec![1], &grammar.rules[1]),
        ]
        .into_iter()
        .collect();

        let cs = CSRepresentation::new(grammar.clone(), 0);
        assert_eq!(
            cs.build_generator().with_fallback(&['A']).0.next(),
            Some(d1)
        );
        assert_eq!(
            cs.build_generator().with_fallback(&['A', 'A']).0.next(),
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
