use nom::IResult;
use num_traits::One;
use std::fmt::Debug;
use std::hash::Hash;
use std::vec::Vec;
use std::str::FromStr;
use recognisable::Transition;
use automata::finite_state_automaton::{FiniteStateAutomaton, FiniteStateInstruction};
use util::parsing::{parse_finals, parse_initial};

impl<Q, T, W> FromStr for FiniteStateAutomaton<Q, T, W>
where
    Q: Clone + FromStr + Hash + Ord + PartialEq,
    Q::Err: Debug,
    T: Clone + Eq + FromStr + Hash + Ord,
    T::Err: Debug,
    W: Clone + Eq + FromStr + One + Ord,
    W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut init = None;
        let mut fin = Vec::new();
        let mut transitions: Vec<Transition<FiniteStateInstruction<Q>, T, W>> = Vec::new();

        let mut it = s.lines();

        while let Some(l) = it.next() {
            if l.trim_start().starts_with("initial:") {
                match parse_initial(l.trim_start().as_bytes()) {
                    IResult::Done(_, result) => {
                        init = Some(result);
                    }
                    _ => return Err(format!("Malformed initial declaration: {}", l)),
                }
            } else if l.trim_start().starts_with("final:") {
                match parse_finals(l.trim_start().as_bytes()) {
                     IResult::Done(_, result) => {
                        fin = result;
                    }
                    _ => return Err(format!("Malformed final declaration: {}", l)),
                }
            } else if !l.is_empty() && !l.trim_start().starts_with("%") {
                transitions.push(l.trim().parse()?);
            }
        }

        match (init, fin) {
            (None, ref r) if r.len() == 0 =>
                Err(format!("No initial state and no final states found.")),
            (None, _) =>
                Err(format!("No initial state found.")),
            (Some(_), ref r) if r.len() == 0 =>
                Err(format!("No final states found.")),
            (Some(i), r) =>
                Ok(FiniteStateAutomaton::new(transitions, i, r)),
        }
    }
}

impl<Q: FromStr> FromStr for FiniteStateInstruction<Q>
where
    Q::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v: Vec<&str> = s.split_whitespace().collect();
        let e: String = "Malformed state.".to_string();
        if v.len() == 3 {
            match v[1] {
                "->" | "→" => {
                    Ok(FiniteStateInstruction {
                        source_state: v[0].parse().map_err(|_| e.clone())?,
                        target_state: v[2].parse().map_err(|_| e.clone())?,
                    })
                },
                _ => {
                    Err(format!("FiniteStateInstruction malformed: {}", s))
                },
            }
        } else {
            Err(format!("FiniteStateInstruction malformed: {}", s))
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_finite_state_automaton_from_str_leading_comment() {
        let automaton_string = "% leading comment\n\
                                initial: 18\n
                                final: [1, 2]\n\n\
                                Transition [] (0 → 1) # 1";
        let _: FiniteStateAutomaton<usize, char, usize> = automaton_string.parse().unwrap();
    }

    #[test]
    fn test_finite_state_automaton_from_str_end_of_line_comment() {
        let automaton_string = "initial: 18 % end-of-line comment 1\n\
                                final: [1, 2]\n\n\
                                Transition [] (1 -> 2) # 1 % end-of-line comment 2";
        let _: FiniteStateAutomaton<usize, char, usize> = automaton_string.parse().unwrap();
    }

    #[test]
    fn test_finite_state_automaton_from_str_trailing_comment() {
        let automaton_string = "initial: 18\n\
                                final: [1, 2]\n\n\
                                Transition [] (1 → 3) # 1\n\
                                % trailing comment";
        let _: FiniteStateAutomaton<usize, char, usize> = automaton_string.parse().unwrap();
    }
}
