use super::{VarT, Composition};
use std::collections::BTreeMap;

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
    use VarT::{Var, T};

    #[test]
    fn test_identify_terminals() {
        let mut tree_map = BTreeMap::new();
        tree_map.insert(vec![], Composition::from(vec![
            vec![Var(0, 0), T("a"), Var(0, 1), T("b")]
        ]));
        tree_map.insert(vec![0], Composition::from(vec![
            vec![Var(1, 0)],
            vec![T("c")]
        ]));
        tree_map.insert(vec![0, 1], Composition::from(vec![
            vec![T("d")]
        ]));

        let mut identified_tree_map = BTreeMap::new();
        identified_tree_map.insert(vec![], Composition::from(vec![
            vec![Var(0, 0), T((vec![], 1)), Var(0, 1), T((vec![], 3))]
        ]));
        identified_tree_map.insert(vec![0], Composition::from(vec![
            vec![Var(1, 0)],
            vec![T((vec![0], 1))]
        ]));
        identified_tree_map.insert(vec![0, 1], Composition::from(vec![
            vec![T((vec![0, 1], 0))]
        ]));

        let mut terminal_map = BTreeMap::new();
        terminal_map.insert((vec![], 1), "a");
        terminal_map.insert((vec![], 3), "b");
        terminal_map.insert((vec![0], 1), "c");
        terminal_map.insert((vec![0, 1], 0), "d");

        assert_eq!((identified_tree_map, terminal_map), identify_terminals(&tree_map));
    }
}