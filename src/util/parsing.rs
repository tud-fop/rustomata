use std::fmt::Debug;
use std::str::{FromStr, from_utf8};
use nom::{IResult, anychar, is_space};

/// Parses a token (i.e. a terminal symbol or a non-terminal symbol).
/// A *token* can be of one of the following two forms:
///
/// * It is a string containing neither of the symbols `'"'`, `' '`, `'-'`, `'→'`, `','`, `';'`, `')'`, `']'`.
/// * It is delimited by the symbol `'#` on both sides and each occurrence of `'\\'` or `'#` inside the delimiters is escaped. TODO should maybe be fixed, but works for now
pub fn parse_token<A: FromStr>(input: &[u8]) -> IResult<&[u8], A>
    where <A as FromStr>::Err: Debug
{
    named!(
        parse_token_s<&str>,
        map_res!(
            alt!(
                delimited!(
                    char!('#'),
                    escaped!(is_not!("\\#"), '\\', anychar),
                    char!('#')
                ) |
                is_not!(" \"-→,;)]")
            ),
            from_utf8
        )
    );

    parse_token_s(input).map(|x| x.parse().unwrap())
}

/// Parses the `input` into a `Vec<A>` given an `inner_parser` for type `A`, an `opening` delimiter, a `closing` delimiter, and a `separator`.
/// The `inner_parser` must not consume the `separator`s or the `closing` delimiter of the given `input`.
pub fn parse_vec<'a, A, P>(input: &'a [u8], inner_parser: P, opening: &str, closing: &str, separator: &str) -> IResult<&'a [u8], Vec<A>>
    where P: Fn(&'a [u8]) -> IResult<&'a [u8], A>
{
    do_parse!(
        input,
        tag!(opening) >>
            take_while!(is_space) >>
            result: many0!(
                do_parse!(
                    opt!(tag!(separator)) >>
                        take_while!(is_space) >>
                        the_token: inner_parser >>
                        take_while!(is_space) >>
                        (the_token)
                )
            ) >>
            tag!(closing) >>
            (result)
    )
}
/// parses initials of the form `initials: [...]` into a vector of type `N`
pub fn parse_initials<N: FromStr>(input: &[u8]) -> IResult<&[u8], Vec<N>>
    where <N as FromStr>::Err: Debug
{
    do_parse!(
        input,
        tag!("initial:") >>
            take_while!(is_space) >>
            result: call!(|x| parse_vec(x, parse_token, "[", "]", ",")) >>
            (result)
    )
}
