use std::collections::BTreeMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

mod from_str;
// mod relabel;

/// Variable or terminal symbol in an MCFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum VarT<T> {
    /// `Var(i, j)` represents the `j`th component of the `i`th successor.
    /// Indexing starts from `0`.
    Var(usize, usize),
    T(T),
}

/// Composition function in an MCFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Composition<T> {
    pub composition: Vec<Vec<VarT<T>>>,
}

impl<T> From<Vec<Vec<VarT<T>>>> for Composition<T> {
    fn from(encapsulated_value: Vec<Vec<VarT<T>>>) -> Self {
        Composition { composition: encapsulated_value }
    }
}

/// Rule of a weighted MCFG.
#[derive(Debug, PartialOrd, Ord, Clone)]
pub struct PMCFGRule<N, T, W> {
    pub head: N,
    pub tail: Vec<N>,
    pub composition: Composition<T>,
    pub weight: W,
}

impl<N, T, W> PMCFGRule<N, T, W>
    where T: Clone,
          W: Clone,
{
    pub fn map_nonterminals<F, M>(&self, f: F) -> PMCFGRule<M, T, W>
        where F: Fn(&N) -> M,
    {
        PMCFGRule {
            head: f(&self.head),
            tail: self.tail.iter().map(f).collect(),
            composition: self.composition.clone(),
            weight: self.weight.clone()
        }
    }
}

/// A weighted MCFG.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct PMCFG<N, T, W> {
    pub _dummy: PhantomData<T>,
    pub initial: Vec<N>,
    pub rules: Vec<PMCFGRule<N, T, W>>,
}

impl<N: Hash, T: Hash, W> Hash for PMCFGRule<N, T, W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.head.hash(state);
        self.tail.hash(state);
        self.composition.hash(state);
    }
}

impl<N: PartialEq, T: PartialEq, W> PartialEq for PMCFGRule<N, T, W> {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.tail == other.tail && self.composition == other.composition
    }
}

impl<N: Eq, T: Eq, W> Eq for PMCFGRule<N, T, W> {}

impl<T: fmt::Display> fmt::Display for Composition<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = "".to_string();

        let mut iter0 = self.composition.iter().peekable();
        let mut iter1;

        buffer.push_str("[");
        while let Some(proj) = iter0.next() {
            iter1 = proj.into_iter().peekable();
            buffer.push_str("[");
            while let Some(vart) = iter1.next() {
                buffer.push_str(format!("{}", vart).as_str());
                if iter1.peek().is_some() {
                    buffer.push_str(", ");
                }
            }
            buffer.push_str("]");
            if iter0.peek().is_some() {
                buffer.push_str(", ");
            }
        }
        buffer.push_str("]");

        write!(f, "{}", buffer)
    }
}

impl<T: fmt::Display> fmt::Display for VarT<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VarT::Var(i, j) => {
                write!(f, "Var {} {}", i, j)
            },
            VarT::T(ref x) => {
                write!(f, "T \"{}\"", x)
            },
        }
    }
}

impl<N: fmt::Display, T: fmt::Display, W: fmt::Display> fmt::Display for PMCFGRule<N, T, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
           let mut buffer = "".to_string();

           let mut iter = self.tail.iter().peekable();

           buffer.push_str("(");
           while let Some(nt) = iter.next() {
               buffer.push_str(format!("\"{}\"", nt).as_str());
               if iter.peek().is_some() {
                   buffer.push_str(", ");
               }
           }
           buffer.push_str(")");

        write!(f, "\"{}\" → {} {}  # {}", self.head, self.composition, buffer, self.weight)
        // write!(f, "\"{}\" → {}", self.head, self.composition)
    }
}

impl<N: fmt::Display, T: fmt::Display, W: fmt::Display> fmt::Display for PMCFG<N, T, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buffer = "".to_string();

        let mut iter = self.initial.iter().peekable();

        buffer.push_str("initial: [");
        while let Some(nt) = iter.next() {
            buffer.push_str(format!("\"{}\"", nt).as_str());
            if iter.peek().is_some() {
                buffer.push_str(", ");
            }
        }
        buffer.push_str("]\n\n");

        for r in &self.rules {
            buffer.push_str(format!("{}\n", r).as_str());
        }

        write!(f, "{}", buffer)
    }
}

pub fn evaluate<T: Clone + fmt::Debug>(term_map: &BTreeMap<Vec<usize>, Composition<T>>) -> Composition<T> {
    evaluate_pos(term_map, vec![])
}

pub fn evaluate_pos<T: Clone + fmt::Debug>(term_map: &BTreeMap<Vec<usize>, Composition<T>>, address: Vec<usize>) -> Composition<T> {
    let unexpanded_composition = &term_map.get(&address).unwrap().composition;
    let mut expanded_nonterminals: BTreeMap<_, Vec<Vec<VarT<T>>>> = BTreeMap::new();
    let mut expanded_composition = Vec::new();

    for component in unexpanded_composition {
        let mut expanded_component = Vec::new();

        for variable in component {
            match variable {
                &VarT::Var(num_nonter, num_compon) => {
                    if !expanded_nonterminals.contains_key(&num_nonter) {
                        let mut child_address = address.clone();
                        child_address.push(num_nonter);
                        let nonter_compos = evaluate_pos(term_map, child_address).composition;
                        expanded_nonterminals.insert(num_nonter, nonter_compos);
                    }

                    let nonter_compos = expanded_nonterminals.get(&num_nonter).unwrap();

                    if let Some(compon) = nonter_compos.get(num_compon) {
                        for terminal in compon {
                            expanded_component.push(terminal.clone());
                        }
                    } else {
                        panic!("{:?}: use of {}-th component of nonterminal {} that has only {} components!",
                               unexpanded_composition, num_compon, num_nonter, nonter_compos.len());
                    }

                },
                &VarT::T(ref terminal) => {
                    expanded_component.push(VarT::T(terminal.clone()));
                },
            }
        }

        expanded_composition.push(expanded_component);
    }

    Composition::from(expanded_composition)
}

pub fn identify_terminals<A: Clone>(tree_map: &BTreeMap<Vec<usize>, Composition<A>>) -> (BTreeMap<Vec<usize>, Composition<(Vec<usize>, usize)>>, BTreeMap<(Vec<usize>, usize), A>) {
    let mut identified_tree_map = BTreeMap::new();
    let mut terminal_map = BTreeMap::new();

    for (address, composition) in tree_map {
        let vec_compos = &composition.composition;
        let mut identified_compos = Vec::new();
        let mut compos_var_pos = 0;

        for component in vec_compos {
            let mut identified_compon = Vec::new();

            for variable in component {
                match variable {
                    &VarT::Var(x, y) => {
                        identified_compon.push(VarT::Var(x, y));
                    },
                    &VarT::T(ref terminal) => {
                        let terminal_id = (address.clone(), compos_var_pos);
                        identified_compon.push(VarT::T(terminal_id.clone()));
                        terminal_map.insert(terminal_id, terminal.clone());
                    },
                };

                compos_var_pos += 1;
            }

            identified_compos.push(identified_compon);
        }

        identified_tree_map.insert(address.clone(), Composition::from(identified_compos));
    }

    (identified_tree_map, terminal_map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use self::VarT::{Var, T};

    #[test]
    fn test_evaluate() {
        let mut term_map: BTreeMap<Vec<usize>, _> = BTreeMap::new();
        term_map.insert(vec![], Composition::from(vec![
                vec![Var(0,0), Var(1,0), Var(0,1), Var(1,1)]
        ]));
        term_map.insert(vec![0], Composition::from(vec![
                vec![Var(1,0), Var(0,0)],
                vec![Var(2,0), Var(0,1)]
        ]));
        term_map.insert(vec![0,0], Composition::from(vec![
                vec![Var(1,0), Var(0,0)],
                vec![Var(2,0), Var(0,1)]
        ]));
        term_map.insert(vec![0,0,0], Composition::from(vec![
                vec![],
                vec![]
        ]));
        term_map.insert(vec![0,0,1], Composition::from(vec![
                vec![T('a')]
        ]));
        term_map.insert(vec![0,0,2], Composition::from(vec![
                vec![T('c')]
        ]));
        term_map.insert(vec![0,1], Composition::from(vec![
                vec![T('a')]
        ]));
        term_map.insert(vec![0,2], Composition::from(vec![
                vec![T('c')]
        ]));
        term_map.insert(vec![1], Composition::from(vec![
                vec![Var(1,0), Var(0,0)],
                vec![Var(2,0), Var(0,1)]
        ]));
        term_map.insert(vec![1,0], Composition::from(vec![
                vec![],
                vec![]
        ]));
        term_map.insert(vec![1,1], Composition::from(vec![
                vec![T('b')]
        ]));
        term_map.insert(vec![1,2], Composition::from(vec![
                vec![T('d')]
        ]));

        let expanded_compos = Composition::from(vec![
            vec![T('a'), T('a'), T('b'), T('c'), T('c'), T('d')]
        ]);

        assert_eq!(expanded_compos, evaluate(&term_map));
    }

    #[test]
    #[should_panic(expected =
        "[[Var(0, 0), Var(0, 1)]]: use of 1-th component of nonterminal 0 that has only 1 components!"
    )]
    fn test_evaluate_invalid_composition() {
        let mut term_map: BTreeMap<Vec<usize>, _> = BTreeMap::new();
        term_map.insert(vec![], Composition::from(vec![
                vec![Var(0,0), Var(0,1)]
        ]));
        term_map.insert(vec![0], Composition::from(vec![
                vec![T('a')]
        ]));

        evaluate(&term_map);
    }

    #[test]
    fn test_identify_terminals() {
        let mut tree_map = BTreeMap::new();
        tree_map.insert(vec![], Composition::from(vec![
            vec![Var(0,0), T("a"), Var(0,1), T("b")]
        ]));
        tree_map.insert(vec![0], Composition::from(vec![
            vec![Var(1,0)],
            vec![T("c")]
        ]));
        tree_map.insert(vec![1,0], Composition::from(vec![
            vec![T("d")]
        ]));

        let mut identified_tree_map = BTreeMap::new();
        identified_tree_map.insert(vec![], Composition::from(vec![
            vec![Var(0,0), T((vec![], 1)), Var(0,1), T((vec![], 3))]
        ]));
        identified_tree_map.insert(vec![0], Composition::from(vec![
            vec![Var(1,0)],
            vec![T((vec![0], 1))]
        ]));
        identified_tree_map.insert(vec![1,0], Composition::from(vec![
            vec![T((vec![1,0], 0))]
        ]));

        let mut terminal_map = BTreeMap::new();
        terminal_map.insert((vec![], 1), "a");
        terminal_map.insert((vec![], 3), "b");
        terminal_map.insert((vec![0], 1), "c");
        terminal_map.insert((vec![1,0], 0), "d");

        assert_eq!((identified_tree_map, terminal_map), identify_terminals(&tree_map));
    }
}
