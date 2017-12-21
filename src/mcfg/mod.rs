pub mod cs_representation;

use pmcfg::{PMCFG, PMCFGRule};

/// A mutliple context-free grammar.
#[derive(Clone, Debug)]
pub struct MCFG<N, T, W> {
    rules: Vec<PMCFGRule<N, T, W>>,
    initial: N,
}

impl<N, T, W> From<PMCFG<N, T, W>> for MCFG<N, T, W> {
    fn from(grammar: PMCFG<N, T, W>) -> Self {
        let PMCFG {
            rules, mut initial, ..
        } = grammar;
        
        assert!(initial.len() == 1);

        MCFG {
            rules: rules,
            initial: initial.remove(0),
        }
    }
}