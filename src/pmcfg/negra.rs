use super::*;
use std::collections::{BTreeMap, VecDeque};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct TermId {
    address: Vec<usize>,
    compos_var_pos: usize,
}

impl From<(Vec<usize>, usize)> for TermId {
    fn from((address, compos_var_pos): (Vec<usize>, usize)) -> TermId {
        TermId { address, compos_var_pos }
    }
}

pub fn identify_terminals<A>(tree_map: &GornTree<Composition<A>>)
        -> (GornTree<Composition<TermId>>, BTreeMap<TermId, A>)
    where A: Clone,
{
    let mut identified_tree_map = GornTree::new();
    let mut terminal_map = BTreeMap::new();

    for (address, composition) in tree_map {
        let vec_compos = composition;
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
                        let terminal_id = TermId { address: address.clone(), compos_var_pos };
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

pub fn to_negra<H, T, W>(tree_map: &GornTree<PMCFGRule<H, T, W>>, sentence_id: usize)
        -> String
    where H: Clone + ToString,
          T: Clone + ToString,
{
    if !meets_negra_criteria(&tree_map) {
        panic!("The given tree does not meet the negra criteria! All rules must either consist \
                only of nonterminals or of exactly one terminal symbol.");
    }

    let negra_vector = to_negra_vector(&tree_map);
    let mut output = format!("#BOS {}\n", sentence_id);

    for (word, tag, parent) in negra_vector {
        output.push_str(&format!("{}\t{}\t--\t--\t{}\n", word, tag, parent));
    }

    output.push_str(&format!("#EOS {}", sentence_id));
    output
}

fn meets_negra_criteria<H, T, W>(tree_map: &GornTree<PMCFGRule<H, T, W>>)
        -> bool
{
    for (_address, rule) in tree_map {
        let &PMCFGRule { head: _, tail: _, ref composition, weight: _ } = rule;
        let mut contains_nonterminal = false;
        let mut contains_terminal = false;

        for component in composition {
            for variable in component {
                match variable {
                    &VarT::Var(_, _) => {
                        if contains_terminal {
                            return false;
                        }

                        contains_nonterminal = true;
                    },
                    &VarT::T(_) => {
                        if contains_nonterminal || contains_terminal {
                            return false;
                        }

                        contains_terminal = true;
                    },
                }
            }
        }
    }

    true
}

fn to_negra_vector<H, T, W>(tree_map: &GornTree<PMCFGRule<H, T, W>>)
        -> Vec<(String, String, usize)>
    where H: Clone + ToString,
          T: Clone + ToString,
{
    let (term_map, nonterminal_map) = to_term(&tree_map);
    let (identified_tree_map, terminal_map) = identify_terminals(&term_map);
    let evaluated_compos = evaluate(&identified_tree_map);

    let mut negra_vector = Vec::new();
    let mut rule_queue = VecDeque::new();
    let mut rule_number_map = GornTree::new();
    let mut rule_counter = 1;

    for component in evaluated_compos {
        for variable in component {
            match variable {
                VarT::Var(_, _) => {
                    panic!("Nonterminals must not appear in a fully evaluated configuration!");
                },
                VarT::T(terminal_id) => {
                    let terminal_symbol = terminal_map.get(&terminal_id).unwrap();
                    let address = terminal_id.address;
                    let rule_label = nonterminal_map.get(&address).unwrap();

                    let mut parent_address = address;
                    let parent_number = if let None = parent_address.pop() {
                        panic!("Terminals must have a nonterminal-only rule as their parent!");
                    } else {
                        get_rule_number(parent_address, &mut rule_queue, &mut rule_number_map,
                                        &mut rule_counter)
                    };
                    negra_vector.push((terminal_symbol.to_string(), rule_label.to_string(), parent_number));
                },
            }
        }
    }

    while let Some((address, rule_number)) = rule_queue.pop_front() {
        let mut parent_address = address.clone();

        let parent_number = if let None = parent_address.pop() {
            0
        } else {
            get_rule_number(parent_address, &mut rule_queue, &mut rule_number_map, &mut rule_counter)
        };

        let rule_label = nonterminal_map.get(&address).unwrap();
        negra_vector.push((format!("#{}", rule_number), rule_label.to_string(), parent_number));
    }

    negra_vector
}

fn get_rule_number(address: Vec<usize>, rule_queue: &mut VecDeque<(Vec<usize>, usize)>, rule_number_map: &mut GornTree<usize>, rule_counter: &mut usize)
        -> usize
{
    if let Some(rule_number) = rule_number_map.get(&address) {
        return *rule_number
    }

    let rule_number = *rule_counter;
    *rule_counter = *rule_counter + 1;

    rule_number_map.insert(address.clone(), rule_number.clone());
    rule_queue.push_back((address, rule_number));
    rule_number
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::tests::*;
    use self::VarT::{Var, T};
    use std::str::FromStr;

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
            vec![Var(0, 0), T(TermId::from((vec![], 1))), Var(0, 1), T(TermId::from((vec![], 3)))]
        ]));
        identified_tree_map.insert(vec![0], Composition::from(vec![
            vec![Var(1, 0)],
            vec![T(TermId::from((vec![0], 1)))]
        ]));
        identified_tree_map.insert(vec![0, 1], Composition::from(vec![
            vec![T(TermId::from((vec![0, 1], 0)))]
        ]));

        let mut terminal_map = BTreeMap::new();
        terminal_map.insert(TermId::from((vec![], 1)), "a");
        terminal_map.insert(TermId::from((vec![], 3)), "b");
        terminal_map.insert(TermId::from((vec![0], 1)), "c");
        terminal_map.insert(TermId::from((vec![0, 1], 0)), "d");

        assert_eq!((identified_tree_map, terminal_map), identify_terminals(&tree_map));
    }

    #[test]
    fn test_identify_terminals_inverse() {
        let (tree_map, _) = to_term(&example_tree_map());
        let (identified_tree_map, terminal_map) = identify_terminals(&tree_map);
        let mut unidentified_tree_map = GornTree::new();

        for (address, composition) in identified_tree_map {
            let mut unidentified_compos = Vec::new();

            for component in composition.composition {
                let mut unidentified_compon: Vec<VarT<char>> = Vec::new();

                for variable in component {
                    match variable {
                        VarT::Var(x, y) => {
                            unidentified_compon.push(VarT::Var(x, y));
                        },
                        VarT::T(terminal_id) => {
                            unidentified_compon.push(
                                VarT::T(terminal_map.get(&terminal_id).unwrap().clone())
                            );
                        },
                    }
                }

                unidentified_compos.push(unidentified_compon);
            }

            unidentified_tree_map.insert(address, Composition::from(unidentified_compos));
        }

        assert_eq!(tree_map, unidentified_tree_map);
    }

    #[test]
    fn test_to_negra_vector() {
        let tree_map = example_tree_map();
        let negra_vector = vec![
            (String::from("a"), String::from("a"), 1),
            (String::from("a"), String::from("a"), 2),
            (String::from("b"), String::from("b"), 3),
            (String::from("c"), String::from("c"), 1),
            (String::from("c"), String::from("c"), 2),
            (String::from("d"), String::from("d"), 3),
            (String::from("#1"), String::from("A"), 4),
            (String::from("#2"), String::from("A"), 1),
            (String::from("#3"), String::from("B"), 4),
            (String::from("#4"), String::from("S"), 0)
        ];

        assert_eq!(negra_vector, to_negra_vector(&tree_map));
    }

    #[test]
    fn test_meets_negra_criteria() {
        let mut tree_map: GornTree<PMCFGRule<String, char, usize>> = GornTree::new();
        tree_map.insert(vec![], PMCFGRule::from_str(
            "S -> [[T a, T b]] () #1"
        ).unwrap());

        assert_eq!(false, meets_negra_criteria(&tree_map));

        tree_map.insert(vec![], PMCFGRule::from_str(
            "S -> [[Var 0 0, T a]] (A) #1"
        ).unwrap());

        assert_eq!(false, meets_negra_criteria(&tree_map));

        tree_map.insert(vec![], PMCFGRule::from_str(
            "S -> [[Var 0 0], [Var 1 0]] (a, b) #1"
        ).unwrap());
        tree_map.insert(vec![], PMCFGRule::from_str(
            "A -> [[T a]] () #1"
        ).unwrap());

        assert_eq!(true, meets_negra_criteria(&tree_map));
    }

    #[test]
    #[should_panic(expected =
        "The given tree does not meet the negra criteria! All rules must either consist \
         only of nonterminals or of exactly one terminal symbol."
    )]
    fn test_to_negra_violated_criteria() {
        let mut tree_map: GornTree<PMCFGRule<String, char, usize>> = GornTree::new();
        tree_map.insert(vec![], PMCFGRule::from_str(
            "S -> [[T a, T b]] () #1"
        ).unwrap());

        to_negra(&tree_map, 0);
    }
}
