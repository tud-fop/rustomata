use nom::{IResult, digit, is_space};
use num_traits::One;
use std::fmt::Debug;
use std::str::{FromStr, from_utf8};

use crate::grammars::pmcfg::{Composition, PMCFG, PMCFGRule, VarT};
use crate::util::parsing::*;

impl<N, T, W> FromStr for PMCFG<N, T, W>
where
    N: FromStr,
    N::Err: Debug,
    T: Clone + FromStr,
    T::Err: Debug,
    W: FromStr + One,
    W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (initial, rules) = initial_rule_grammar_from_str(s)?;

        Ok(PMCFG { initial, rules })
    }
}

impl<N, T, W> FromStr for PMCFGRule<N, T, W>
where
    N: FromStr,
    N::Err: Debug,
    T: Clone + FromStr,
    T::Err: Debug,
    W: FromStr + One,
    W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_pmcfg_rule(s.as_bytes()) {
            IResult::Done(_, result) => Ok(result),
            _ => Err(format!("Could not parse {}", s)),
        }
    }
}

fn parse_successors<N>(input: &[u8]) -> IResult<&[u8], Vec<N>>
where
    N: FromStr,
    N::Err: Debug,
{
    parse_vec(input, parse_token, "(", ")", ",")
}

fn parse_pmcfg_rule<N, T, W>(input: &[u8]) -> IResult<&[u8], PMCFGRule<N, T, W>>
where
    N: FromStr,
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
            tail: parse_successors >>
            take_while!(is_space) >>
            weight_o: opt!(
                complete!(
                    do_parse!(
                        tag!("#") >>
                            take_while!(is_space) >>
                            weight_s: map_res!(is_not!(" "), from_utf8) >>
                            (weight_s.parse().unwrap())
                    )
                )
            ) >>
            take_while!(is_space) >>
            many0!(tag!("%")) >>
            take_while!(|_| true) >>
            (PMCFGRule {
                head: head,
                tail: tail,
                composition: Composition { composition: composition },
                weight: weight_o.unwrap_or(W::one()),
            })
    )
}

fn parse_var_t<T>(input: &[u8]) -> IResult<&[u8], VarT<T>>
where
    T: FromStr,
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
where
    T: FromStr,
    T::Err: Debug,
{
    parse_vec(input, parse_var_t, "[", "]", ",")
}

fn parse_composition<T>(input: &[u8]) -> IResult<&[u8], Vec<Vec<VarT<T>>>>
where
    T: FromStr,
    T::Err: Debug,
{
    parse_vec(input, parse_projection, "[", "]", ",")
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_pmcfg_from_str_leading_comment() {
        let grammar_string = "% leading comment\n\
                              initial: [S]\n\n\
                              S → [[T a]] ()";
        let _: PMCFG<char, char, usize> = grammar_string.parse().unwrap();
    }

    #[test]
    fn test_pmcfg_from_str_end_of_line_comment() {
        let grammar_string = "initial: [S] % end-of-line comment 1\n\n\
                              S → [[T a]] () % end-of-line comment 2";
        let _: PMCFG<char, char, usize> = grammar_string.parse().unwrap();
    }

    #[test]
    fn test_pmcfg_from_str_trailing_comment() {
        let grammar_string = "initial: [S]\n\n\
                              S → [[T a]] ()\n\
                              % trailing comment";
        let _: PMCFG<char, char, usize> = grammar_string.parse().unwrap();
    }
}
