use std::hash::Hash;
use std::vec::Vec;
use std::str::FromStr;
use std::num::ParseIntError;

use recognisable::Transition;
use tree_stack_automaton::{TreeStack, TreeStackAutomaton, TreeStackInstruction};

impl<A, T, W> FromStr for TreeStackAutomaton<A, T, W>
    where A: Clone + FromStr + Hash + Ord + PartialEq,
          T: Clone + Eq + FromStr + Hash + Ord,
          W: Clone + Eq + FromStr + Ord,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let initial: A;
        let mut transitions: Vec<Transition<TreeStackInstruction<A>, T, W>>
                = Vec::new();

        let mut it = s.lines();

        match it.next() {
            Some(l) if l.starts_with("initial: ") => {
                initial = l[8..]
                    .trim()
                    .parse()
                    .map_err(|_| format!("Substring {} is not a storage symbol.", l[8..].trim()))?
            }
            _ => return Err("No initial state supplied on line 1.".to_string()),
        }

        for l in s.lines() {
            if l.starts_with("Transition ") {
                transitions.push(l.trim()
                    .parse()
                    .map_err(|_| format!("Substring {} is not a transition.", l.trim()))?);
            }
        }

        Ok(TreeStackAutomaton::new(transitions, TreeStack::new(initial)))
    }
}

impl<A: FromStr> FromStr for TreeStackInstruction<A> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v: Vec<&str> = s.split_whitespace().collect();
        let e: String = "Malformed node label.".to_string();
        match v[0] {
            "Up" if v.len() == 5 => {
                let n: usize = v[1].parse().map_err(|e: ParseIntError| e.to_string())?;
                let cur_val: A = v[2].parse().map_err(|_| e.clone())?;
                let old_val: A = v[3].parse().map_err(|_| e.clone())?;
                let new_val: A = v[4].parse().map_err(|_| e.clone())?;
                Ok(TreeStackInstruction::Up {
                    n,
                    current_val: cur_val,
                    old_val,
                    new_val,
                })
            }
            "Push" if v.len() == 4 => {
                let n: usize = v[1].parse().map_err(|e: ParseIntError| e.to_string())?;
                let cur_val: A = v[2].parse().map_err(|_| e.clone())?;
                let new_val: A = v[3].parse().map_err(|_| e.clone())?;
                Ok(TreeStackInstruction::Push {
                    n,
                    current_val: cur_val,
                    new_val,
                })
            }
            "Down" if v.len() == 4 => {
                let cur_val: A = v[1].parse().map_err(|_| e.clone())?;
                let old_val: A = v[2].parse().map_err(|_| e.clone())?;
                let new_val: A = v[3].parse().map_err(|_| e.clone())?;
                Ok(TreeStackInstruction::Down {
                    current_val: cur_val,
                    old_val,
                    new_val,
                })
            }
            _ => Err("Malformed instruction.".to_string()),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_tree_stack_automaton_from_str_leading_comment() {
        let automaton_string = "% leading comment\n\
                                initial: 18\n\n\
                                Transition [] (Push 0 1 2) # 1";
        let _: TreeStackAutomaton<char, char, usize> = automaton_string.parse().unwrap();
    }

    #[test]
    fn test_tree_stack_automaton_from_str_end_of_line_comment() {
        let automaton_string = "initial: 18 % end-of-line comment 1\n\n\
                                Transition [] (Push 0 1 2) # 1 % end-of-line comment 2";
        let _: TreeStackAutomaton<char, char, usize> = automaton_string.parse().unwrap();
    }

    #[test]
    fn test_tree_stack_automaton_from_str_trailing_comment() {
        let automaton_string = "initial: 18\n\n\
                                Transition [] (Push 0 1 2) # 1\n\
                                % trailing comment";
        let _: TreeStackAutomaton<char, char, usize> = automaton_string.parse().unwrap();
    }
}
