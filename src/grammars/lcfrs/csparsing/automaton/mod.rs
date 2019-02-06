use num_traits::One;
use std::{collections::{BinaryHeap}, ops::Mul, mem::replace, hash::Hash, default::Default};
use vecmultimap::VecMultiMap;
use integeriser::{HashIntegeriser, Integeriser};
use grammars::{pmcfg::{PMCFGRule, VarT}, lcfrs::csparsing::{BracketContent, Bracket}};
use util::factorizable::Factorizable;
use fnv::FnvHashMap;
use num_traits::Zero;

mod chart;
mod kbest;
mod estimates;
mod rulemask;

use self::chart::DenseChart;
use self::kbest::ChartIterator;
pub use self::estimates::SxOutside;
pub use self::rulemask::RuleMaskBuilder;

pub type RuleIdT = u32;
pub type StateT = u32;
pub type RangeT = u8;

/// stores a lhs state and weight of a rule
pub type BuRule<W> = (W, StateT);
/// stores rhs states, rule identifier and rule weight for binary rules
pub type TdBinary<W> = (RuleIdT, StateT, StateT, W);
/// stores rhs state, rule identifier and rule weight for chain rules
pub type TdUnary<W> = (RuleIdT, StateT, W);
/// stores rule identifier and weight of a terminal rule
pub type TdNullary<W> = (RuleIdT, W);
/// Stores the following brackets associated with a single (binarized) rule:
/// * outer bracket,
/// * left bracket, and
/// * right bracket;
/// some of them may be BracketContent::Ignore.
pub type TdBrackets = (BracketContent, BracketContent, BracketContent);
/// non-existent integerized state
pub static NOSTATE: u32 = -1i32 as StateT;

/// Stores binarized rules of the context-free approximation of an lcfrs
/// with brackets of the chomsky-schützenberger construction.
/// These rules are stored indexed by left rhs nonterminal (bottom-up) and
/// lhs nonterminal (top-down).
#[derive(Debug, Serialize, Deserialize)]
pub struct Automaton<T: Eq + Hash, W> (
    // rules indexed by successor nonterminals and terminals for bottom-up processing
    Vec<Vec<(RuleIdT, StateT, BuRule<W>)>>,   // binary compatability: left state -> [(right state, action)]
    Vec<Vec<(RuleIdT, BuRule<W>)>>,             // unary wraps: state -> [action]
    FnvHashMap<T, Vec<(RuleIdT, BuRule<W>)>>,   // terminals: termial -> [action]
    // rules indexed by lhs nonterminal for top-down processing
    Vec<Vec<TdBinary<W>>>,
    Vec<Vec<TdUnary<W>>>,
    Vec<Vec<TdNullary<W>>>,
    // misc data
    Vec<Vec<(StateT, W, StateT)>>,        // mirrored binary compatability map;
                                          // this is only used for the inside weight
    StateT,                               // initial
    Vec<TdBrackets>,                      // stores the brackets associated with each rule
);

/// intermediate representation for states:
/// * either nonterminal with component index,
/// * or unique state indexed by rule, component and position
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum State<Nt> { Non(Nt, usize), Unique(RuleIdT, usize, usize) }

impl<T: Eq + Hash, W> Automaton<T, W> {
    /// extracts an `Automaton` from a given grammar with integerized rules
    pub fn from_grammar<'a, N>(rules: impl Iterator<Item=(RuleIdT, &'a PMCFGRule<N, T, W>)>, init: N) -> Self
    where
        T: 'a + Clone,
        W: 'a + Factorizable + Mul<Output=W> + One + Copy,
        N: 'a + Hash + Eq,
    {
        use self::State::*;
        use self::BracketContent::*;

        let mut bu_initials: FnvHashMap<T, Vec<(RuleIdT, BuRule<W>)>> = FnvHashMap::default();
        let mut bu_unaries = VecMultiMap::new();
        let mut bu_binaries = VecMultiMap::new();
        let mut td_initials = VecMultiMap::new();
        let mut td_unaries = VecMultiMap::new();
        let mut td_binaries = VecMultiMap::new();
        let mut rule_to_brackets = Vec::new();

        let mut state_integerizer = HashIntegeriser::new();

        for (rule_id, rule) in rules {
            let weight_factors = rule.weight.factorize(rule.composition.len());
            for (component_id, (component, component_weight)) in rule.composition.iter().zip(weight_factors).enumerate() {
                let component_t = Component(rule_id as u32, component_id as u8);
                let state_i = state_integerizer.integerise(Non(&rule.head, component_id)) as StateT;

                // handle terminal rules and chain rules
                if component.len() == 1 {
                    match component[0] {
                        VarT::T(ref t) => {
                            bu_initials.entry(t.clone()).or_default().push((rule_to_brackets.len() as RuleIdT, (component_weight, state_i)));
                            td_initials.push_to(state_i as usize, (rule_to_brackets.len() as RuleIdT, component_weight));
                            rule_to_brackets.push((component_t, Terminal, Ignore));
                        },
                        VarT::Var(i, j) => {
                            let variable_t = Variable(rule_id as u32, i as u8, j as u8);
                            let successor_state_i = state_integerizer.integerise(Non(&rule.tail[i], j)) as StateT;
                            bu_unaries.push_to(successor_state_i as usize, (rule_to_brackets.len() as RuleIdT, (component_weight, state_i)));
                            td_unaries.push_to(state_i as usize, (rule_to_brackets.len() as RuleIdT, successor_state_i, component_weight));
                            rule_to_brackets.push((component_t, variable_t, Ignore));
                        }
                    }
                }   // handle rules with multiple nonnterminals
                else {
                    let v1 = component[0].unwrap_var();
                    let v2 = component[1].unwrap_var();
                    let successors = ( state_integerizer.integerise(Non(&rule.tail[v1.0], v1.1)) as StateT
                                     , state_integerizer.integerise(Non(&rule.tail[v2.0], v2.1)) as StateT
                                     );
                    let vt1 = Variable(rule_id as u32, v1.0 as u8, v1.1 as u8);
                    let vt2 = Variable(rule_id as u32, v2.0 as u8, v2.1 as u8);
                    // set lhs nontermal as target state or create unique
                    // state if there are nontermals left
                    let (mut targetstate, mut outer_t) = if component.len() == 2 {
                        (state_i, component_t)
                    } else {
                        (state_integerizer.integerise(Unique(rule_id, component_id, 0)) as StateT, Ignore)
                    };  
                    bu_binaries.push_to(successors.0 as usize, (rule_to_brackets.len() as RuleIdT, successors.1, (component_weight, targetstate)));
                    td_binaries.push_to(targetstate as usize, (rule_to_brackets.len() as RuleIdT, successors.0, successors.1, component_weight));
                    rule_to_brackets.push((outer_t, vt1, vt2));
                    // continue for the remaining nonterminals in the same manner
                    for succ_offset in 1..=(component.len()-2) {
                        let l_successor = targetstate;
                        let v = component[1+succ_offset].unwrap_var();
                        let vt = Variable(rule_id as u32, v.0 as u8, v.1 as u8);
                        let r_successor = state_integerizer.integerise(Non(&rule.tail[v.0], v.1)) as StateT;
                        if component.len() == 2+succ_offset { 
                            targetstate = state_i;
                            outer_t = component_t;
                        } else {
                            targetstate = state_integerizer.integerise(Unique(rule_id, component_id, succ_offset)) as StateT;
                            outer_t = Ignore;
                        };
                        bu_binaries.push_to(l_successor as usize, (rule_to_brackets.len() as RuleIdT, r_successor, (W::one(), targetstate)));
                        td_binaries.push_to(targetstate as usize, (rule_to_brackets.len() as RuleIdT, l_successor, r_successor, W::one()));
                        rule_to_brackets.push((outer_t, Ignore, vt));
                    }
                }
            }
        }

        let binaries = bu_binaries.into_vec_with_size(state_integerizer.size());
        let mut binaries_mirrorred = VecMultiMap::new();
        for (ql, qr, w, q0) in binaries.iter().enumerate().flat_map(|(ql, v)| v.iter().map(move |&(_, qr, (w, q0))| (ql, qr, w, q0))) {
            binaries_mirrorred.push_to(qr as usize, (ql as StateT, w, q0));
        }
        Automaton(
            binaries,
            bu_unaries.into_vec_with_size(state_integerizer.size()),
            bu_initials,
            td_binaries.into_vec_with_size(state_integerizer.size()),
            td_unaries.into_vec_with_size(state_integerizer.size()),
            td_initials.into_vec_with_size(state_integerizer.size()),
            binaries_mirrorred.into_vec_with_size(state_integerizer.size()),
            state_integerizer.integerise(Non(&init, 0)) as StateT,
            rule_to_brackets
        )
    }

    /// Create an Iteator for well bracketed words in the  language of the
    /// context-free approximation
    pub fn generate<'a>(&'a self, word: &[T], beam: usize, delta: W, estimates: &SxOutside<W>, rulefilter: Vec<bool>) -> ChartIterator<'a, W>
    where
        W: Ord + Copy + Mul<Output=W> + Zero + One,
    {
        let chart = self.fill_chart(word, beam, delta, estimates, &rulefilter);
        ChartIterator::new(chart, self, rulefilter)
    }

    pub fn states(&self) -> usize {
        self.0.len()
    }
}

impl<T: Eq + Hash, W: Ord + Mul<Output=W> + Copy + Zero + One> Automaton<T, W> {
    /// implements the CKY algorithm with chain rules
    pub fn fill_chart(&self, word: &[T], beam: usize, delta: W, outsides: &SxOutside<W>, rule_filter: &[bool]) -> DenseChart<W> {
        let n = word.len();
        let nonterminals = self.0.len();

        // contains the constituents ordered by weight
        let mut heap_of_nonterminals: BinaryHeap<(W, StateT)> = BinaryHeap::with_capacity(beam);
        let mut chart = DenseChart::new(n, nonterminals, beam);

        for range in 1..=n {
            for l in 0..=(n-range) {
                let r = l + range;

                heap_of_nonterminals.clear();

                // initial predictions for each position in word
                if range == 1 {
                    if let Some(initials) = self.2.get(&word[l]) {
                        heap_of_nonterminals.extend(initials.iter().filter_map(
                            |&(rid, (w, q))| {
                                if !rule_filter[rid as usize] { return None; }
                                let _ = outsides.get(q, l, r, n)?;
                                Some((w, q))
                            }
                        ))
                    }
                }
                
                // binary step
                for mid in l+1..r {
                    for &(lnt, lew) in chart.iterate_nont(l as u8, mid as u8) {
                        let available_rules = &self.0[lnt as usize];
                        let mut cache = (NOSTATE, None);
                        heap_of_nonterminals.extend(available_rules.iter().filter_map(
                            |&(rid, rnt, (ruw, lhs))| {
                                if !rule_filter[rid as usize] { return None; }
                                let riw = if cache.0 == rnt { cache.1 }
                                          else { chart.get_weight(mid as u8, r as u8, rnt) }?;
                                let _ = outsides.get(lhs, l, r, n)?;
                                Some((lew * ruw * riw, lhs))
                            }
                        ));
                    }
                }

                // unary step and insertion into chart
                let mut skip = vec![false; self.0.len()];
                let mut i = beam;
                let mut worst_weight = delta * heap_of_nonterminals.peek().map_or(W::zero(), |&(w, _)| w);
                while let Some((w, q)) = heap_of_nonterminals.pop() {
                    if replace(&mut skip[q as usize], true) { continue; }
                    chart.add_entry(l as u8, r as u8, q, w);
                    heap_of_nonterminals.extend(self.1[q as usize].iter().filter_map(
                        |&(rid, (rw, q))| {
                            if !rule_filter[rid as usize] { return None; }
                            let _ = outsides.get(q, l, r, n)?;
                            Some((rw * w, q))
                        }
                    ));
                    i -= 1;
                    if i == 0 || w < worst_weight { break; }
                }
            }
        }

        chart
    }
}