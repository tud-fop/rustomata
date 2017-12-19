use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::{FromStr, from_utf8};

use nom::{IResult, is_space, digit};

use pmcfg::{VarT, Composition, PMCFGRule, PMCFG};
use util::parsing::*;

impl<N, T, W> FromStr for PMCFG<N, T, W>
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
        Ok(PMCFG {
            _dummy: PhantomData,
            initial: initial,
            rules: rules,
        })

    }
}


impl<N, T, W> FromStr for PMCFGRule<N, T, W>
    where N: FromStr,
          N::Err: Debug,
          T: Clone + FromStr,
          T::Err: Debug,
          W: FromStr,
          W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_pmcfg_rule(s.as_bytes()) {
            IResult::Done(_, result) => Ok(result),
            _                        => Err(format!("Could not parse {}", s))
        }
    }
}


fn parse_successors<N>(input: &[u8]) -> IResult<&[u8], Vec<N>>
    where N: FromStr,
          N::Err: Debug,
{
    parse_vec(input, parse_token, "(", ")", ",")
}


fn parse_pmcfg_rule<N, T, W>(input: &[u8]) -> IResult<&[u8], PMCFGRule<N, T, W>>
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
            tail: parse_successors >>
            take_while!(is_space) >>
            tag!("#") >>
            take_while!(is_space) >>
            weight_s: map_res!(is_not!(" "), from_utf8) >>
            (PMCFGRule {
                head: head,
                tail: tail,
                composition: Composition { composition: composition },
                weight: weight_s.parse().unwrap(),
            })
    )
}


fn parse_var_t<T>(input: &[u8]) -> IResult<&[u8], VarT<T>>
    where T: FromStr,
          T::Err: Debug,
{
    do_parse!(
        input,
        result: alt!(
            do_parse!(
                tag!("Var") >>
                    take_while!(is_space) >>
                    i: digit >>
                    take_while!(is_space) >>
                    j: digit >>
                    (VarT::Var(
                        from_utf8(i).unwrap().parse().unwrap(),
                        from_utf8(j).unwrap().parse().unwrap(),
                    ))
            ) |
            do_parse!(
                tag!("T") >>
                    take_while!(is_space) >>
                    token: parse_token >>
                    (VarT::T(token))
            )
        ) >>
            (result)
    )
}


fn parse_projection<T>(input: &[u8]) -> IResult<&[u8], Vec<VarT<T>>>
    where T: FromStr,
          T::Err: Debug,
{
    parse_vec(input, parse_var_t, "[", "]", ",")
}

fn parse_composition<T>(input: &[u8]) -> IResult<&[u8], Vec<Vec<VarT<T>>>>
    where T: FromStr,
          T::Err: Debug,
{
    parse_vec(input, parse_projection, "[", "]", ",")
}
