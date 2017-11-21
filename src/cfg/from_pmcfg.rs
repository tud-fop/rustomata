use std::fmt::Debug;
use std::marker::PhantomData;

use pmcfg::*;
use cfg::{LetterT, CFGComposition, CFGRule, CFG};

impl<N: Clone + Debug + Ord + PartialEq,
     T: Clone + Debug + Ord + PartialEq,
     W: Clone + Debug + Ord + PartialEq> From<PMCFG<N, T, W>> for CFG<N, T, W> {
    fn from(pmcfg: PMCFG<N, T, W>) -> CFG<N, T, W> {
        let mut rules = Vec::new();

        for r in pmcfg.rules{
            if r.composition.composition.len() != 1 {
                panic!("[ERROR] Too many clauses: {:?}", r)
            }

            let mut new_composition = Vec::new();

            let r_0 = &r.composition.composition[0];

            for v in r_0 {
                match *v {
                    VarT::T(ref x)=>{
                        new_composition.push(
                            LetterT::Value(x.clone())
                        );
                    },
                    VarT::Var(i,0)=>{
                        let x = &r.tail[i as usize];
                        new_composition.push(
                            LetterT::Label(x.clone())
                        );
                    },
                    _=> panic!("[ERROR] Access to wrong component: {:?} in {:?}", v, r_0),
                }

            }
            rules.push(CFGRule{
                head: r.head.clone(),
                composition: CFGComposition{
                    composition: new_composition,
                },
                weight: r.weight.clone(),
            });
        }

        CFG { _dummy: PhantomData,
               initial: pmcfg.initial.clone(),
               rules: rules,
        }
    }
}
