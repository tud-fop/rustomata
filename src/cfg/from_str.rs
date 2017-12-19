use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::{FromStr, from_utf8};

use nom::{IResult, is_space};

use cfg::{LetterT, CFGComposition, CFGRule, CFG};
use util::parsing::*;

impl<N, T, W> FromStr for CFG<N, T, W>
    where N: FromStr,
          N::Err: Debug,
          T: Clone + FromStr,
          T::Err: Debug,
          W: FromStr,
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
                        => return Err(format!("Malformed declaration of initial nonterminals: {}", l))
                }
            },
            _ => return Err("Given string is empty.".to_string())
        }

        for l in s.lines() {
            if !l.is_empty() && !l.starts_with("initial: ") {
                rules.push(try!(l.trim().parse()));
            }
        }

        Ok(CFG {
            _dummy: PhantomData,
            initial: initial,
            rules: rules,
        })

    }
}

impl<N, T, W> FromStr for CFGRule<N, T, W>
    where N: FromStr,
          N::Err: Debug,
          T: Clone + FromStr,
          T::Err: Debug,
          W: FromStr,
          W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_cfg_rule(s.as_bytes()) {
            IResult::Done(_, result) => Ok(result),
            _                        => Err(format!("Could not parse {}", s))
        }
    }
}

fn parse_cfg_rule<N, T, W>(input: &[u8]) -> IResult<&[u8], CFGRule<N, T, W>>
    where N: FromStr,
          N::Err: Debug,
          T: FromStr,
          T::Err: Debug,
          W: FromStr,
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
            tag!("#") >>
            take_while!(is_space) >>
            weight_s: map_res!(is_not!(" "), from_utf8) >>
            (CFGRule {
                head: head,
                composition: CFGComposition { composition: composition },
                weight: weight_s.parse().unwrap(),
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
