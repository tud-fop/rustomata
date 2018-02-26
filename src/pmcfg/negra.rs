use super::*;
use std::collections::{BTreeMap, VecDeque};

pub fn identify_terminals<A>(tree_map: &GornTree<Composition<A>>)
        -> (GornTree<Composition<(Vec<usize>, usize)>>, BTreeMap<(Vec<usize>, usize), A>)
    where A: Clone,
{
    let mut identified_tree_map = GornTree::new();
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

pub fn to_negra<H, T, W>(tree_map: &GornTree<PMCFGRule<H, T, W>>, sentence_num: usize)
        -> String
    where H: Clone + ToString,
          T: Clone + ToString,
{
    let negra_vector = to_negra_vector(&tree_map);
    let mut output = format!("#BOS {}\n", sentence_num);

    for (symbol1, symbol2, number) in negra_vector {
        output.push_str(&format!("{}\t{}\t{}\n", symbol1, symbol2, number));
    }

    output.push_str(&format!("#EOS {}", sentence_num));
    output
}

pub fn to_negra_vector<H, T, W>(tree_map: &GornTree<PMCFGRule<H, T, W>>)
        -> Vec<(String, String, usize)>
    where H: Clone + ToString,
          T: Clone + ToString,
{
    let (term_map, nonterminal_map) = to_term(&tree_map);
    // TODO: Enforce negra grammar restrictions
    let (identified_tree_map, terminal_map) = identify_terminals(&term_map);
    let evaluated_compos = evaluate(&identified_tree_map);

    let mut negra_vector = Vec::new();
    let mut rule_queue = VecDeque::new();
    let mut finished_map = GornTree::new();
    let mut rule_counter = 0;

    for component in evaluated_compos.composition {
        for variable in component {
            match variable {
                VarT::Var(_, _) => {
                    panic!("Nonterminals must not appear in a fully evaluated configuration!");
                },
                VarT::T(terminal_id) => {
                    let terminal_symbol = terminal_map.get(&terminal_id).unwrap();
                    let ref address = terminal_id.0;
                    let rule_label = nonterminal_map.get(address).unwrap();
                    let rule_number = get_rule_number(address, &mut rule_queue, &finished_map,
                                                      &mut rule_counter);
                    negra_vector.push((terminal_symbol.to_string(), rule_label.to_string(), rule_number));
                }
            }
        }
    }

    while let Some((address, rule_number)) = rule_queue.pop_front() {
        let mut parent_address = address.clone();

        let parent_number = if let None = parent_address.pop() {
            0
        } else {
            let number = get_rule_number(&parent_address, &mut rule_queue, &finished_map, &mut rule_counter);
            finished_map.insert(parent_address.clone(), number);
            number
        };

        let rule_label = nonterminal_map.get(&address).unwrap();
        negra_vector.push((rule_number.to_string(), rule_label.to_string(), parent_number));
    }

    negra_vector
}

fn get_rule_number(address: &Vec<usize>, rule_queue: &mut VecDeque<(Vec<usize>, usize)>, finished_map: &GornTree<usize>, rule_counter: &mut usize)
        -> usize
{
    if let Some(rule_number) = finished_map.get(address) {
        return *rule_number
    }

    let rule_number = *rule_counter;
    *rule_counter = *rule_counter + 1;
    rule_queue.push_back((address.clone(), rule_number));
    rule_number
}

#[cfg(test)]
mod tests {
    use super::*;
    use self::VarT::{Var, T};

    #[test]
    fn test_identify_terminals() {
        let mut tree_map = GornTree::new();
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

        let mut identified_tree_map = GornTree::new();
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
