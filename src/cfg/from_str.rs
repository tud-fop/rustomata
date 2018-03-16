use nom::{IResult, is_space};
use num_traits::One;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::{FromStr, from_utf8};

use cfg::{CFG, CFGComposition, CFGRule, LetterT};
use util::parsing::*;

impl<N, T, W> FromStr for CFG<N, T, W>
    where N: FromStr,
          N::Err: Debug,
          T: Clone + FromStr,
          T::Err: Debug,
          W: FromStr + One,
          W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.lines();
        let mut rules = Vec::new();
        let initial;

        match it.next() {
            Some(l) => {
                match parse_initials(l.as_bytes()) {
                    IResult::Done(_, result)
                        => initial = result,
                    _
                        => return Err(format!("Malformed declaration of initial nonterminals: \'{}\'", l))
                }
            },
            _ => return Err(String::from("Cannot parse an empty string as a CFG!"))
        }

        while let Some(l) = it.next() {
            if !l.is_empty() {
                rules.push(l.trim().parse()?);
            }
        }

        Ok(CFG {
            _dummy: PhantomData,
            initial,
            rules,
        })
    }
}

impl<N, T, W> FromStr for CFGRule<N, T, W>
    where N: FromStr,
          N::Err: Debug,
          T: Clone + FromStr,
          T::Err: Debug,
          W: FromStr + One,
          W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_cfg_rule(s.as_bytes()) {
            IResult::Done(_, result) => Ok(result),
            _                        => Err(format!("Could not parse \'{}\'", s))
        }
    }
}

fn parse_cfg_rule<N, T, W>(input: &[u8]) -> IResult<&[u8], CFGRule<N, T, W>>
    where N: FromStr,
          N::Err: Debug,
          T: FromStr,
          T::Err: Debug,
          W: FromStr + One,
          W::Err: Debug,
{
    do_parse!(
        input,
        head: parse_token >>
        take_while!(is_space) >>
        alt!(tag!("→") | tag!("->") | tag!("=>")) >>
        take_while!(is_space) >>
        composition: parse_composition >>
        take_while!(is_space) >>
        weight_o: opt!(
            complete!(
                do_parse!(
                    tag!("#") >>
                    take_while!(is_space) >>
                    weight_s: map_res!(is_not!(" "), from_utf8) >>
                    weight: expr_res!(weight_s.parse()) >>
                    (weight)
                )
            )
        ) >>
        take_while!(is_space) >>
        alt!(
            eof!() |
            preceded!(tag!("%"), take_while!(|_| true))
        ) >>
        (CFGRule {
            head: head,
            composition: CFGComposition { composition: composition },
            weight: weight_o.unwrap_or(W::one()),
        })
    )
}

fn parse_letter_t<N, T>(input: &[u8]) -> IResult<&[u8], LetterT<N,T>>
    where N: FromStr,
          N::Err: Debug,
          T: FromStr,
          T::Err: Debug,
{
    do_parse!(
        input,
        result: alt!(
            do_parse!(
                tag!("Nt") >>
                take_while!(is_space) >>
                token: parse_token >>
                (LetterT::Label(token))
            ) |
            do_parse!(
                tag!("T") >>
                take_while!(is_space) >>
                token: parse_token >>
                (LetterT::Value(token))
            )
        ) >>
        (result)
    )
}

fn parse_composition<N, T>(input: &[u8]) -> IResult<&[u8], Vec<LetterT<N,T>>>
    where N: FromStr,
          N::Err: Debug,
          T: FromStr,
          T::Err: Debug,
{
    parse_vec(input, parse_letter_t, "[", "]", ",")
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_cfg_from_str_leading_comment() {
        let grammar = "% leading comment\n\
                       initial: [S]\n\n\
                       S → [T a]";
        let _: CFG<char, char, usize> = grammar.parse().unwrap();
    }

    #[test]
    fn test_cfg_from_str_end_of_line_comment() {
        let grammar = "initial: [S] % end-of-line comment 1\n\n\
                       S → [T a] % end-of-line comment 2";
        let _: CFG<char, char, usize> = grammar.parse().unwrap();
    }

    #[test]
    fn test_cfg_from_str_trailing_comment() {
        let grammar = "initial: [S]\n\n\
                       S → [T a]\n\
                       % trailing comment";
        let _: CFG<char, char, usize> = grammar.parse().unwrap();
    }

    #[test]
    fn test_parse_letter_t_legal_input() {
        let legal_inputs = vec![
            ("Nt S xyz", " xyz", LetterT::Label('S')),
            ("Nt  S", "", LetterT::Label('S')),
            ("T a xyz", " xyz", LetterT::Value('a')),
        ];

        for (legal_input, control_rest, control_parsed) in legal_inputs {
            assert_eq!(
                (control_rest.as_bytes(), control_parsed),
                parse_letter_t::<char, char>(legal_input.as_bytes()).unwrap()
            );
        }
    }

    #[test]
    fn test_parse_letter_t_illegal_input() {
        let illegal_inputs = vec![
            " Nt 1",
            "nt 1",
            "t 1",
            "Nt:1",
            "Nt a",
            "T a",
        ];

        for illegal_input in illegal_inputs {
            match parse_letter_t::<u8, u8>(illegal_input.as_bytes()) {
                IResult::Done(_, _) | IResult::Incomplete(_) =>
                    panic!("Was able to parse the illegal input \'{}\'", illegal_input),
                IResult::Error(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_letter_t_incomplete_input() {
        let incomplete_inputs = vec![
            "Nt",
            "T",
        ];

        for incomplete_input in incomplete_inputs {
            match parse_letter_t::<char, char>(incomplete_input.as_bytes()) {
                IResult::Done(_, _) | IResult::Error(_) =>
                    panic!("The input was not handled as incomplete: \'{}\'", incomplete_input),
                IResult::Incomplete(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_cfg_rule_legal_input() {
        let rule = CFGRule {
            head: 'S',
            composition: CFGComposition { composition: vec![LetterT::Value('a')] },
            weight: 1.0
        };
        let legal_inputs = vec![
            ("S → [T a] # 1 % comment", "", rule.clone()),
            ("S  →    [T a]#1 %comment", "", rule.clone()),
            ("S → [T a] # 1.0", "", rule.clone()),
            ("S → [T a]", "", rule.clone()),
            ("S → [T a] % comment", "", rule.clone()),
            ("S -> [T a] # 1", "", rule.clone()),
            ("S => [T a] # 1", "", rule.clone()),
        ];

        for (legal_input, control_rest, control_parsed) in legal_inputs {
            assert_eq!(
                (control_rest.as_bytes(), control_parsed),
                parse_cfg_rule::<char, char, f32>(legal_input.as_bytes()).unwrap()
            );
        }
    }

    #[test]
    fn test_parse_cfg_rule_illegal_input() {
        let illegal_inputs = vec![
            " S → [T a] # 1 % comment",
            "S [T a] # 1 % comment",
            "S → [T a] # 1 comment",
            "S ~> [T a] # 1",
            "AB → [T a] # 1 % comment",
            "S → [T a] # a % comment",
            "S → [T a] #",
        ];

        for illegal_input in illegal_inputs {
            match parse_cfg_rule::<char, char, f32>(illegal_input.as_bytes()) {
                IResult::Done(_, _) | IResult::Incomplete(_) =>
                    panic!("Was able to parse the illegal input \'{}\'", illegal_input),
                IResult::Error(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_cfg_rule_incomplete_input() {
        let incomplete_inputs = vec![
            "S →",
            "S",
        ];

        for incomplete_input in incomplete_inputs {
            match parse_cfg_rule::<char, char, f32>(incomplete_input.as_bytes()) {
                IResult::Done(_, output) =>
                    panic!("The input was not handled as incomplete: \'{}\'", output),
                IResult::Error(error) => panic!("Error with \'{}\'; {:?}", incomplete_input, error),
                IResult::Incomplete(_) => (),
            }
        }
    }

    #[test]
    fn test_cfg_rule_from_str_legal_input() {
        let control_rule = CFGRule {
            head: 'S',
            composition: CFGComposition { composition: vec![LetterT::Value('a')] },
            weight: 1.0
        };

        assert_eq!(
            Ok(control_rule),
            CFGRule::from_str("S → [T a]")
        );
    }

    #[test]
    fn test_cfg_rule_from_str_illegal_input() {
        let incomplete_or_illegal_inputs = vec![
            "S → [T 1",
            "S → [T a]",
        ];

        for input in incomplete_or_illegal_inputs {
            assert_eq!(
                Err(format!("Could not parse \'{}\'", &input)),
                CFGRule::<u8, u8, f32>::from_str(input)
            );
        }
    }

    #[test]
    fn test_cfg_from_str_legal_input() {
        let input = "initial: [S]\n\n\
                     S → [T a, Nt S, T b] # 0.4\n\
                     S → []               # 0.6";

        let rule_s0 = CFGRule {
            head: 'S',
            composition: CFGComposition { composition: vec![
                LetterT::Value('a'), LetterT::Label('S'), LetterT::Value('b'),
            ] },
            weight: 0.4
        };
        let rule_s1 = CFGRule {
            head: 'S',
            composition: CFGComposition { composition: vec![]},
            weight: 0.6
        };
        let control_grammar = CFG {
            _dummy: PhantomData,
            initial: vec!['S'],
            rules: vec![rule_s0, rule_s1]
        };

        assert_eq!(
            control_grammar,
            CFG::from_str(input).unwrap()
        );
    }

    #[test]
    fn test_cfg_from_str_illegal_input() {
        assert_eq!(
            Err(String::from("Cannot parse an empty string as a CFG!")),
            CFG::<u8, u8, u8>::from_str("")
        );

        let malformed_initial = "initial: [a]";
        assert_eq!(
            Err(format!("Malformed declaration of initial nonterminals: \'{}\'", &malformed_initial)),
            CFG::<u8, u8, u8>::from_str(malformed_initial)
        );

        let malformed_rule = "initial: [0]\n\n\
                              S → [T a]";
        assert_eq!(
            Err(String::from("Could not parse \'S → [T a]\'")),
            CFG::<u8, u8, u8>::from_str(malformed_rule)
        );
    }
}
