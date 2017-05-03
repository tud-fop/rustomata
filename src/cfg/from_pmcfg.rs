use std::fmt::Debug;
use std::marker::PhantomData;

pub use pmcfg::*;
use cfg::{LetterT, CFGComposition, CFGRule, CFG};


pub fn from_pmcfg<N: Clone + Debug + Ord + PartialEq,
     T: Clone + Debug + Ord + PartialEq,
     W: Clone + Debug + Ord + PartialEq
     >(pmcfg :PMCFG<N, T, W>) -> Result<CFG<N,T,W>, String> {
        let mut rules = Vec::new();

        for r in pmcfg.rules{
            if r.composition.composition.len()!=1{
                return Err(format!("Too many clauses"))
            }

            let mut new_composition = Vec::new();

            let r_0 = &r.composition.composition[0];

            for v in r_0{
                match v{
                    &VarT::T(ref x)=>{
                        new_composition.push(
                            LetterT::Value(x.clone())
                        );
                    },
                    &VarT::Var(i,0)=>{
                        let x = &r.tail[i as usize];
                        new_composition.push(
                            LetterT::Label(x.clone())
                        );
                    },
                    _=> return Err(format!("Could not compile rules")),
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


        Ok(CFG {
            _dummy: PhantomData,
            initial: pmcfg.initial.clone(),
            rules: rules,
        })

}
