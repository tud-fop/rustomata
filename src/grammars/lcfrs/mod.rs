use grammars::pmcfg::PMCFGRule;
use grammars::pmcfg::VarT;
use std::hash::Hash;
use std::collections::HashMap;

mod conversion;
mod from_str;
pub mod from_discodop;
pub mod from_rparse;
pub mod csparsing;


/// A linear context-free rewriting system.
#[derive(Debug, Clone)]
pub struct Lcfrs<N, T, W> {
    rules: Vec<PMCFGRule<N, T, W>>,
    init: N,
}

impl<N, T, W> Lcfrs<N, T, W>
where
    N: Hash + Eq,
{
    /// Instantiates a new LCFRS from a list of rules.
    /// The rules and nonterminals are checked for consistency with regards to
    /// * consistent fanouts,
    /// * linear and non-deleting compositions, and
    /// * fanout 1 for initial nonterminals.
    pub fn new(rules: Vec<PMCFGRule<N, T, W>>, init: N) -> Option<Self> {
        {
            // scope for lieftime of `fanouts` that borrows from rules
            let fanouts = read_fanouts(&rules)?;

            // check initial fanout
            if fanouts.get(&&init) != Some(&1) {
                return None;
            }

            // check variables
            if !rules.iter().all(|rule| {
                let fanouts: Option<Vec<usize>> = rule.tail
                    .iter()
                    .map(|nt| fanouts.get(nt).map(|u| *u))
                    .collect();
                if let Some(fanouts) = fanouts {
                    check_composition(&rule.composition.composition, &fanouts)
                } else {
                    false
                }
            })
            {
                return None;
            }
        }

        Some(Lcfrs { rules, init })
    }
}

impl<N, T, W> Lcfrs<N, T, W> {
    /// Deconstructs the data type into its parts.
    pub fn destruct(self) -> (Vec<PMCFGRule<N, T, W>>, N) {
        (self.rules, self.init)
    }
}

impl<N: fmt::Display, T: fmt::Display, W: fmt::Display> fmt::Display for Lcfrs<N, T, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = "".to_string();

        buffer.push_str(format!("initial: [{}]\n\n", self.init).as_str());

        for r in &self.rules {
            buffer.push_str(format!("{}\n", r).as_str());
        }

        write!(f, "{}", buffer)
    }
}

/// Reads the fanout off `PMCFGRules`.
/// Will return `None` if rule fanouts are inconsistent for same nonterminals.
fn read_fanouts<'a, R, N, T, W>(rules: R) -> Option<HashMap<&'a N, usize>>
where
    R: IntoIterator<Item = &'a PMCFGRule<N, T, W>>,
    N: 'a + Hash + Eq,
    T: 'a,
    W: 'a,
{
    let mut fanouts: HashMap<&N, usize> = HashMap::new();
    for rule in rules {
        if fanouts.entry(&rule.head).or_insert_with(|| {
            rule.composition.composition.len()
        }) != &rule.composition.composition.len()
        {
            return None;
        }
    }
    Some(fanouts)
}

/// Checks a composition for linearity.
/// Will return true if each variable occurs exactly once according to the given fanouts.
fn check_composition<T>(composition: &Vec<Vec<VarT<T>>>, fanouts: &[usize]) -> bool {
    use util::vec_entry;

    let mut variable_occurances: Vec<Vec<usize>> = Vec::new();
    for (i, j) in composition
        .iter()
        .flat_map(|component| component.iter())
        .filter_map(|symbol| match *symbol {
            VarT::Var(i, j) => Some((i, j)),
            _ => None,
        })
    {
        vec_entry(&mut variable_occurances, i).push(j);
    }

    variable_occurances.len() == fanouts.len() &&
        variable_occurances.into_iter().enumerate().all(
            |(i, mut js)| {
                js.sort();
                let js_: Vec<usize> = (0..fanouts[i]).collect();
                js == js_
            },
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use grammars::pmcfg::{Composition, VarT};
    use std::collections::BTreeSet;
    use grammars::mcfg::Mcfg;

    #[test]
    fn fanouts() {
        assert_eq!(
            read_fanouts(&lcfrs_rules()),
            Some(vec![(&1, 1), (&2, 2)].into_iter().collect())
        );

        assert_eq!(read_fanouts(&inconsistent_rules()), None);
    }

    #[test]
    fn lcfrs_checks() {
        assert!(Lcfrs::new(lcfrs_rules(), 1).is_some());
        assert!(Lcfrs::new(inconsistent_rules(), 1).is_none());
        assert!(Lcfrs::new(mcfg_rules(), 1).is_none());
    }

    #[test]
    fn conversion() {
        let lcfrs: Lcfrs<(usize, BTreeSet<usize>), usize, ()> = Mcfg::new(mcfg_rules(), 1).into();
        
        let emptyset = BTreeSet::new();
        let set0: BTreeSet<_> = vec![0].into_iter().collect();
        let set01: BTreeSet<_> = vec![0, 1].into_iter().collect();
        
        assert_eq!(
            lcfrs.init,
            (1, BTreeSet::new())
        );

        let rules: Vec<PMCFGRule<(usize, BTreeSet<usize>), usize, ()>> =
            vec![
                PMCFGRule {
                    weight: (),
                    head: (1, emptyset.clone()),
                    tail: vec![],
                    composition: Composition { composition: vec![vec![VarT::T(0)]] },
                },
                PMCFGRule {
                    weight: (),
                    head: (1, emptyset.clone()),
                    tail: vec![(2, set0.clone())],
                    composition: Composition { composition: vec![vec![VarT::Var(0, 0), VarT::T(0)]] },
                },
                PMCFGRule {
                    weight: (),
                    head: (2, set0.clone()),
                    tail: vec![(2, set01.clone())],
                    composition: Composition { composition: vec![vec![VarT::T(2)]] },
                },
                PMCFGRule {
                    weight: (),
                    head: (2, set01.clone()),
                    tail: vec![(2, set01.clone())],
                    composition: Composition { composition: vec![] },
                },
            ];

        assert_eq!(
            lcfrs.rules.into_iter().collect::<BTreeSet<_>>(),
            rules.into_iter().collect::<BTreeSet<_>>()
        )
    }

    fn mcfg_rules() -> Vec<PMCFGRule<usize, usize, ()>> {
        vec![
            PMCFGRule {
                weight: (),
                head: 2,
                tail: vec![2],
                composition: Composition { composition: vec![vec![VarT::T(1)], vec![VarT::T(2)]] },
            },
            PMCFGRule {
                weight: (),
                head: 1,
                tail: vec![2],
                composition: Composition { composition: vec![vec![VarT::Var(0, 1), VarT::T(0)]] },
            },
            PMCFGRule {
                weight: (),
                head: 1,
                tail: vec![],
                composition: Composition { composition: vec![vec![VarT::T(0)]] },
            },
        ]
    }

    fn inconsistent_rules() -> Vec<PMCFGRule<usize, usize, ()>> {
        vec![
            PMCFGRule {
                weight: (),
                head: 2,
                tail: vec![2],
                composition: Composition { composition: vec![vec![VarT::T(1)], vec![VarT::T(2)]] },
            },
            PMCFGRule {
                weight: (),
                head: 1,
                tail: vec![2],
                composition: Composition {
                    composition: vec![vec![VarT::Var(0, 0)], vec![VarT::Var(0, 1)]],
                },
            },
            PMCFGRule {
                weight: (),
                head: 1,
                tail: vec![],
                composition: Composition { composition: vec![vec![VarT::T(0)]] },
            },
        ]
    }

    fn lcfrs_rules() -> Vec<PMCFGRule<usize, usize, ()>> {
        vec![
            PMCFGRule {
                weight: (),
                head: 2,
                tail: vec![],
                composition: Composition { composition: vec![vec![VarT::T(1)], vec![VarT::T(2)]] },
            },
            PMCFGRule {
                weight: (),
                head: 1,
                tail: vec![2],
                composition: Composition {
                    composition: vec![vec![VarT::Var(0, 0), VarT::Var(0, 1)]],
                },
            },
            PMCFGRule {
                weight: (),
                head: 1,
                tail: vec![1],
                composition: Composition { composition: vec![vec![VarT::Var(0, 0)]] },
            },
            PMCFGRule {
                weight: (),
                head: 1,
                tail: vec![],
                composition: Composition { composition: vec![vec![VarT::T(0)]] },
            },
        ]
    }
}
