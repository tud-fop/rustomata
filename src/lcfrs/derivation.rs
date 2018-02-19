use std::iter::{once, repeat};
use PMCFGRule;
use log_domain::LogDomain;
use std::fmt;
use std::collections::BTreeMap;

/// A derivation tree of PMCFG rules.
#[derive(PartialEq)]
pub struct Derivation<'a, N: 'a, T: 'a>(
    pub BTreeMap<Vec<usize>, &'a PMCFGRule<N, T, LogDomain<f64>>>,
);

/// Displays a derivation tree Haskell-style.
impl<'a, N: 'a + fmt::Display, T: 'a + fmt::Display> fmt::Debug for Derivation<'a, N, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = String::new();
        let &Derivation(ref tree) = self;

        for (pos, rule) in tree {
            if !pos.is_empty() {
                let pipes: Vec<String> = repeat("|  ".to_string())
                    .take(pos.len())
                    .chain(once("\n".to_string()))
                    .chain(repeat("|  ".to_string()).take(pos.len() - 1))
                    .chain(once("+- ".to_string()))
                    .collect();
                buffer.push_str(&pipes.join(""));
            }
            buffer.push_str(format!("{}\n", rule).as_str());
        }
        write!(f, "{}", buffer)
    }
}

fn descend_terminals<'a, N, T>(
    terminal_list: &mut Vec<(&'a T, String, usize)>,
    position: Vec<usize>,
    component: usize,
    parent_id: usize,
    derivation: &Derivation<'a, N, T>,
    ids: &BTreeMap<Vec<usize>, usize>,
) where
    N: 'a + ToString,
    T: 'a,
{
    use VarT;
    
    let rule = *derivation.0.get(&position).unwrap();
    let id = *ids.get(&position).unwrap();

    for symbol in &rule.composition.composition[component] {
        match *symbol {
            VarT::T(ref t) => {
                let constituent = rule.head.to_string().split("_").next().unwrap().to_owned();
                terminal_list.push((t, constituent, parent_id));
            }
            VarT::Var(i, j) => {
                let mut successor = position.to_owned();
                successor.push(i);
                descend_terminals(terminal_list, successor, j, id, derivation, ids);
            }
        }
    }
}

/// Implements the export format for constituent trees.
impl<'a, N, T> fmt::Display for Derivation<'a, N, T>
where
    N: 'a + fmt::Display + ToString,
    T: 'a + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let id_via_position: BTreeMap<Vec<usize>, usize> =
            self.0.keys().cloned().zip(once(0).chain(500..)).collect();

        let mut terminal = Vec::new();
        let nonterminal: Vec<_> = self.0
            .keys()
            .filter_map(|position| {
                let rule = self.0.get(position).unwrap();
                if !position.is_empty() && !rule.tail.is_empty() {
                    let (_, parent_position) = position.split_last()?;
                    let parent_id = id_via_position.get(parent_position)?;
                    let id = id_via_position.get(position)?;

                    // remove splitting annotations from constituents
                    let constituent = rule.head.to_string().split("_").next()?.to_owned();
                    
                    Some((id, constituent, parent_id))
                } else {
                    None
                }
            })
            .collect();

        descend_terminals(&mut terminal, Vec::new(), 0, 0, &self, &id_via_position);

        for (symbol, constituent, parent_id) in terminal {
            writeln!(f, "{}\t{}\t--\t--\t{}", symbol, constituent, parent_id)?;
        }
        for (id, constituent, parent_id) in nonterminal {
            writeln!(f, "#{}\t{}\t--\t--\t{}", id, constituent, parent_id)?;
        }

        Ok(())
    }
}
