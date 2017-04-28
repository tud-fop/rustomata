use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::FromStr;

pub use pmcfg::*;
use cfg::{LetterT, Composition, CFGRule, CFG};

impl<N: FromStr + Clone, T: FromStr + Clone, W: FromStr + Clone> FromStr for CFG<N, T, W>
    where <N as FromStr>::Err: Debug, <T as FromStr>::Err: Debug, <W as FromStr>::Err: Debug {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pmcfg : PMCFG<N, T, W> = s.parse().unwrap();
        let mut rules = Vec::new();

        if pmcfg.initial.len()!=1{
            return Err(format!("Too many initials"))
        }

        for r in pmcfg.rules{
            if r.composition.composition.len()!=1{
                return Err(format!("Too many ......")) //TODO name
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
                    _=> return Err(format!("Could not compile rules")), //TODO name
                }

            }
            rules.push(CFGRule{
                head: r.head.clone(),
                composition: Composition{
                    composition: new_composition,
                },
                weight: r.weight.clone(),
            });
        }


        Ok(CFG {
            _dummy: PhantomData,
            initial: pmcfg.initial[0].clone(),
            rules: rules,
        })

    }
}
