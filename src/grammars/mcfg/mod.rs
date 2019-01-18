use grammars::pmcfg::{PMCFG, PMCFGRule};
use grammars::lcfrs::Lcfrs;

/// A mutliple context-free grammar.
#[derive(Clone, Debug)]
pub struct Mcfg<N, T, W> {
    rules: Vec<PMCFGRule<N, T, W>>,
    initial: N,
}

impl<N, T, W> Mcfg<N, T, W> {
    pub fn new(rules: Vec<PMCFGRule<N, T, W>>, initial: N) -> Self {
        Mcfg { initial, rules }
    }
    pub fn destruct(self) -> (Vec<PMCFGRule<N, T, W>>, N) {
        (self.rules, self.initial)
    }
}

impl<N, T, W> From<Lcfrs<N, T, W>> for Mcfg<N, T, W> {
    fn from(lcfrs: Lcfrs<N, T, W>) -> Self {
        let (rules, initial) = lcfrs.destruct();
        Mcfg { rules, initial }
    }
}

impl<N, T, W> Mcfg<N, T, W> {
    /// As long as `TryFrom` is unstable ...
    pub fn from_pmcfg(mut pmcfg: PMCFG<N, T, W>) -> Option<Self> {
        use std::collections::BTreeSet;
        use grammars::pmcfg::VarT;

        for rule in &pmcfg.rules {
            // check variables, return none if one occurs more than once
            let mut variables: BTreeSet<(usize, usize)> = BTreeSet::new();
            for (i, j) in rule.composition.composition.iter().flat_map(|component| {
                component.iter().filter_map(
                    |s| if let VarT::Var(i, j) = *s {
                        Some((i, j))
                    } else {
                        None
                    },
                )
            })
            {
                if !variables.insert((i, j)) {
                    return None;
                }
            }
        }

        // only one initial nonterminal for now, sorry!
        if pmcfg.initial.len() != 1 {
            None
        } else {
            Some(Mcfg {
                rules: pmcfg.rules,
                initial: pmcfg.initial.pop().unwrap(),
            })
        }

    }
}
