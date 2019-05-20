use nom::{is_space, IResult};
use num_traits::One;
use std::fmt::Debug;
use std::str::{from_utf8, FromStr};
use std::vec::Vec;

use crate::recognisable::{Instruction, Transition};
use crate::util::parsing::{parse_comment, parse_token, parse_vec};

impl<I: Instruction + FromStr, T: FromStr, W: FromStr> FromStr for Transition<I, T, W>
where
    I: FromStr,
    I::Err: Debug,
    T: FromStr,
    T::Err: Debug,
    W: FromStr + One,
    W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_transition(s.as_bytes()) {
            IResult::Done(_, result) => Ok(result),
            _ => Err(format!("Could not parse: {}", s)),
        }
    }
}

fn parse_transition<I, T, W>(input: &[u8]) -> IResult<&[u8], Transition<I, T, W>>
where
    I: FromStr,
    I::Err: Debug,
    T: FromStr,
    T::Err: Debug,
    W: FromStr + One,
    W::Err: Debug,
{
    do_parse!(
        input,
        tag!("Transition")
            >> take_while!(is_space)
            >> word: parse_word
            >> take_while!(is_space)
            >> instruction:
                map_res!(
                    delimited!(tag!("("), take_until!(")"), tag!(")")),
                    from_utf8
                )
            >> take_while!(is_space)
            >> weight_o:
                opt!(complete!(do_parse!(
                    tag!("#")
                        >> take_while!(is_space)
                        >> weight_s: map_res!(is_not!(" "), from_utf8)
                        >> (weight_s.parse().unwrap())
                )))
            >> take_while!(is_space)
            >> opt!(complete!(parse_comment))
            >> (Transition {
                word: word,
                weight: weight_o.unwrap_or(W::one()),
                instruction: instruction.parse().unwrap(),
            })
    )
}

fn parse_word<T>(input: &[u8]) -> IResult<&[u8], Vec<T>>
where
    T: FromStr,
    T::Err: Debug,
{
    parse_vec(input, parse_token, "[", "]", ",")
}

#[test]
fn test_parse_word() {
    let legal_inputs = vec![
        ("[]", "", vec![]),
        (
            "[a, b, c]",
            "",
            vec![String::from("a"), String::from("b"), String::from("c")],
        ),
        (
            "[xyz, ab]",
            "",
            vec![String::from("xyz"), String::from("ab")],
        ),
        (
            "[\"xyz\", \"ab\", cd] bla",
            " bla",
            vec![String::from("xyz"), String::from("ab"), String::from("cd")],
        ),
    ];

    for (legal_input, control_rest, control_parsed) in legal_inputs {
        assert_eq!(
            (control_rest.as_bytes(), control_parsed),
            parse_word(legal_input.as_bytes()).unwrap()
        );
    }
}

#[test]
fn test_parse_transitions() {
    let legal_inputs = vec![
        (
            "Transition [1, 2, 3] (1) # 2 % blub",
            "",
            Transition {
                word: vec![1usize, 2usize, 3usize],
                weight: 2usize,
                instruction: 1usize,
            },
        ),
        (
            "Transition [1, 2, 3] (1) % blub",
            "",
            Transition {
                word: vec![1usize, 2usize, 3usize],
                weight: 1usize,
                instruction: 1usize,
            },
        ),
        (
            "Transition [1, 2, 3] (1)",
            "",
            Transition {
                word: vec![1usize, 2usize, 3usize],
                weight: 1usize,
                instruction: 1usize,
            },
        ),
    ];

    for (legal_input, control_rest, control_parsed) in legal_inputs {
        assert_eq!(
            IResult::Done(control_rest.as_bytes(), control_parsed),
            parse_transition(legal_input.as_bytes())
        );
    }
}
