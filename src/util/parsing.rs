use nom::{IResult, is_space};
use std::fmt::Debug;
use std::str::{FromStr, from_utf8};


/// Parses a token (i.e. a terminal symbol or a non-terminal symbol).
/// A *token* can be of one of the following two forms:
///
/// * It is a string containing neither of the symbols `'"'`, `' '`, `'-'`, `'→'`, `','`, `';'`, `')'`, `']'`, `'%'`.
/// * It is delimited by the symbol `'"'` on both sides and each occurrence of `'\\'` or `'"'` inside the delimiters is escaped.
pub fn parse_token<A>(input: &[u8])
        -> IResult<&[u8], A>
    where A: FromStr,
          A::Err: Debug,
{
    named!(
        parse_token_s<&str>,
        map_res!(
            alt!(
                delimited!(
                    tag!("\""),
                    escaped!(is_not!("\"\\"), '\\', one_of!("\\\"")),
                    tag!("\"")
                ) |
                is_not!(" \\\"-→,;)]%#")
            ),
            from_utf8
        )
    );

    do_parse!(
        input,
        output: parse_token_s >>
        token: expr_res!(output.parse()) >>
        (token)
    )
}

/// Parses the `input` into a `Vec<A>` given an `inner_parser` for type `A`, an `opening` delimiter, a `closing` delimiter, and a `separator`.
/// The `inner_parser` must not consume the `separator`s or the `closing` delimiter of the given `input`.
pub fn parse_vec<'a, A, P>(input: &'a [u8], inner_parser: P, opening: &str, closing: &str, separator: &str)
        -> IResult<&'a [u8], Vec<A>>
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


/// Parses a string of the form `initials: ⟨token⟩` as a the initial symbol of type `I`.
pub fn parse_initial<I>(input: &[u8])
                         -> IResult<&[u8], I>
where I: FromStr,
      I::Err: Debug,
{
    do_parse!(
        input,
        tag!("initial:") >>
            take_while!(is_space) >>
            result: parse_token >>
            take_while!(|_| true) >>
            (result)
    )
}


/// Parses a string of the form `initials: [...]` as a vector of initial symbols of type `I`.
pub fn parse_initials<I>(input: &[u8])
        -> IResult<&[u8], Vec<I>>
    where I: FromStr,
          I::Err: Debug,
{
    do_parse!(
        input,
        tag!("initial:") >>
            take_while!(is_space) >>
            result: call!(|x| parse_vec(x, parse_token, "[", "]", ",")) >>
            (result)
    )
}


/// Consumes any string that begins with the character `%`.
pub fn parse_comment(input: &[u8]) -> IResult<&[u8], ()> {
    do_parse!(
        input,
        tag!("%") >>
            take_while!(|_| true) >>
            (())
    )
}


/// Parses a string as a grammar of initial symbols of type `I` and rules of type `R`.
/// The syntax of the initials must comply with `parse_initials`; the syntax of the rules is solely
/// determined by their type.
/// If the string contains multiple definitions of initials, then the initials of the grammar are
/// going to be the union of all defined initials.
pub fn initial_rule_grammar_from_str<I, R>(s: &str)
        -> Result<((Vec<I>, Vec<R>)), String>
    where I: FromStr,
          I::Err: Debug,
          R: FromStr,
          String: From<R::Err>,
{
    let mut it = s.lines();
    let mut initial = Vec::new();
    let mut rules: Vec<R> = Vec::new();

    while let Some(l) = it.next() {
        if l.trim_left().starts_with("initial:") {
            match parse_initials(l.as_bytes()) {
                IResult::Done(_, mut result) =>
                    initial.append(&mut result),
                _ =>
                    return Err(format!("Malformed declaration of initial nonterminals: \'{}\'", l))
            }
        } else if !l.is_empty() && !l.trim_left().starts_with("%") {
            rules.push(l.trim().parse()?);
        }
    }

    Ok((initial, rules))
}


#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_parse_comment_legal_input() {
        let legal_inputs = vec![
            "% some comment",
            "%% another comment",
        ];

        for legal_input in legal_inputs {
            assert_eq!(
                ("".as_bytes(), ()),
                parse_comment(legal_input.as_bytes()).unwrap(),
            )
        }
    }

    #[test]
    fn test_parse_comment_illegal_input() {
        let illegal_inputs = vec![
            " % not a comment",
            "something else % not a comment",
        ];

        for illegal_input in illegal_inputs {
            match parse_comment(illegal_input.as_bytes()) {
                IResult::Done(_, _) | IResult::Incomplete(_) =>
                    panic!("Was able to parse the illegal input \'{}\'", illegal_input),
                IResult::Error(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_token_legal_input() {
        let legal_inputs = vec![
            ("abcxyz", "", String::from("abcxyz")),
            ("abc,xyz", ",xyz", String::from("abc")),
            ("\"abc\"xyz", "xyz", String::from("abc")),
            ("\"a\\\\b\\\"c\"xyz", "xyz", String::from("a\\\\b\\\"c")),
            ("\"\"\"", "\"", String::from("")),
        ];

        for (legal_input, control_rest, control_parsed) in legal_inputs {
            assert_eq!(
                (control_rest.as_bytes(), control_parsed),
                parse_token::<String>(legal_input.as_bytes()).unwrap()
            );
        }
    }

    #[test]
    fn test_parse_token_illegal_input() {
        let illegal_inputs = vec![
            " xyz",
            "-xyz",
            "→xyz",
            ",xyz",
            ";xyz",
            ")xyz",
            "]xyz",
            "\"\\\"",
            " \"a\"",
        ];

        for illegal_input in illegal_inputs {
            match parse_token::<String>(illegal_input.as_bytes()) {
                IResult::Done(_, _) =>
                    panic!("Was able to parse the illegal input \'{}\'", illegal_input),
                IResult::Error(_) | IResult::Incomplete(_) => (),
            }
        }

        let illegal_inputs = vec![
            "a",
            "\"a\"",
        ];

        for illegal_input in illegal_inputs {
            match parse_token::<u8>(illegal_input.as_bytes()) {
                IResult::Done(_, _) =>
                    panic!("Was able to parse the illegal input \'{}\'", illegal_input),
                IResult::Error(_) | IResult::Incomplete(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_token_incomplete_input() {
        let incomplete_inputs = vec![
            "\"a",
        ];

        for incomplete_input in incomplete_inputs {
            match parse_token::<String>(incomplete_input.as_bytes()) {
                IResult::Done(_, _) | IResult::Error(_) =>
                    panic!("The input was not handled as incomplete: \'{}\'", incomplete_input),
                IResult::Incomplete(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_vec_legal_input() {
        let legal_inputs = vec![
            ("[]xyz", "xyz", vec![]),
            ("[a, bc, d]xyz", "xyz",
                vec![String::from("a"), String::from("bc"), String::from("d")]),
            ("[  a, b ,c]xyz", "xyz",
                vec![String::from("a"), String::from("b"), String::from("c")]),
        ];

        for (legal_input, control_rest, control_parsed) in legal_inputs {
            assert_eq!(
                (control_rest.as_bytes(), control_parsed),
                parse_vec(legal_input.as_bytes(), parse_token, "[", "]", ",").unwrap()
            );
        }
    }

    #[test]
    fn test_parse_vec_illegal_input() {
        let illegal_inputs = vec![
            "(0)",
            "[0]",
            "[0, 1; 2)",
            " []",
            "[a)",
        ];

        for illegal_input in illegal_inputs {
            match parse_vec::<u8, _>(illegal_input.as_bytes(), parse_token, "[", ")", ",") {
                IResult::Done(_, _) | IResult::Incomplete(_) =>
                    panic!("Was able to parse the illegal input \'{}\'", illegal_input),
                IResult::Error(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_vec_incomplete_input() {
        let incomplete_inputs = vec![
            "[a",
            "["
        ];

        for incomplete_input in incomplete_inputs {
            match parse_vec::<String, _>(incomplete_input.as_bytes(), parse_token, "[", "]", ",") {
                IResult::Done(_, _) | IResult::Error(_) =>
                    panic!("The input was not handled as incomplete: \'{}\'", incomplete_input),
                IResult::Incomplete(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_initial_legal_input() {
        let legal_inputs = vec![
            ("initial: xyz % some comment", "", String::from("xyz")),
            ("initial: \"% xy\"", "", String::from("% xy")),
            ("initial:  xyz something else", "", String::from("xyz")),
        ];

        for (legal_input, control_rest, control_parsed) in legal_inputs {
            assert_eq!(
                (control_rest.as_bytes(), control_parsed),
                parse_initial(legal_input.as_bytes()).unwrap()
            );
        }
    }

    #[test]
    fn test_parse_initials_legal_input() {
        let legal_inputs = vec![
            ("initial: [a]xyz", "xyz", vec![String::from("a")]),
            ("initial:  []xyz", "xyz", vec![]),
        ];

        for (legal_input, control_rest, control_parsed) in legal_inputs {
            assert_eq!(
                (control_rest.as_bytes(), control_parsed),
                parse_initials(legal_input.as_bytes()).unwrap()
            );
        }
    }

    #[test]
    fn test_parse_initials_illegal_input() {
        let illegal_inputs = vec![
            "initials: []xyz",
            " initial: []xyz",
        ];

        for illegal_input in illegal_inputs {
            match parse_initials::<String>(illegal_input.as_bytes()) {
                IResult::Done(_, _) | IResult::Incomplete(_) =>
                    panic!("Was able to parse the illegal input \'{}\'", illegal_input),
                IResult::Error(_) => (),
            }
        }
    }

    #[test]
    fn test_parse_initials_incomplete_input() {
        let incomplete_inputs = vec![
            "init",
            "initial: [",
        ];

        for incomplete_input in incomplete_inputs {
            match parse_initials::<String>(incomplete_input.as_bytes()) {
                IResult::Done(_, _) | IResult::Error(_) =>
                    panic!("The input was not handled as incomplete: \'{}\'", incomplete_input),
                IResult::Incomplete(_) => (),
            }
        }
    }
}
