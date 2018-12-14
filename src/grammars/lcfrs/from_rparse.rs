use nom::*;
use std::str::FromStr;
use std::fmt::Debug;

#[derive(Debug, PartialEq)]
enum Rhs<N> {
    Binary(N, N),
    Chain(N)
}
type Yield = Vec<Vec<bool>>;

#[derive(Debug)]
struct RparseClauses<N, W>(Vec<(W, N, Rhs<N>, Yield)>);

fn is_token(c: char) -> bool { !c.is_whitespace() }

named!(parse_rparse_yield<&str, Yield>,
    delimited!(
        tag!("[["),
        separated_list_complete!(
            tag!(", "),
            delimited!(
                tag!("["),
                separated_list_complete!(
                    tag!(", "),
                    map!(
                        alt!(tag!("true") | tag!("false")),
                        |s| if s == "true" { true } else { false }
                    )
                ),
                tag!("]")
            )
        ),
        tag!("]]")
    )
);

fn parse_rparse_clauses<N, W>(s: &str) -> IResult<&str, RparseClauses<N, W>>
where
    N: FromStr,
    W: FromStr,
    <N as FromStr>::Err: Debug,
    <W as FromStr>::Err: Debug,
{
    map!(
        s,
        separated_nonempty_list_complete!(
            tag!("\n"),
            do_parse!(
                terminated!(digit, space)                                                   >>
                weight: take_while1!(|c| is_token(c) && c != ':')                           >>
                tag!(":")                                                                   >>
                lhs: take_while1!(is_token)                                                 >>
                delimited!(space, tag!("-->"), space)                                       >>
                rhs1: take_while1!(is_token)                                                >>
                space                                                                       >>
                o_rhs2: opt!(delimited!(not!(tag!("[[[")), take_while1!(is_token), space))  >>
                y: complete!(parse_rparse_yield)                                            >>
                (weight.parse().unwrap(), lhs.parse().unwrap(), if let Some(rhs2) = o_rhs2 { Rhs::Binary(rhs1.parse().unwrap(), rhs2.parse().unwrap()) } else { Rhs::Chain(rhs1.parse().unwrap()) }, y)
            )
        ),
        |v| RparseClauses(v)
    )
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn rparse_yield () {
        let ys = vec![
            ("[[[false]]]", vec![vec![false]]),
            ("[[[false], [true]]]", vec![vec![false], vec![true]]),
            ("[[[false, true, false], [false], [true, false, false]]]", vec![vec![false, true, false], vec![false], vec![true, false, false]]),
        ];
        for (s, y) in ys {
            eprintln!("{:?}", parse_rparse_yield(s));
            assert_eq!(
                y,
                parse_rparse_yield(s).unwrap().1
            );
        }
    }

    #[test]
    fn rparse_clauses () {
        let ds = vec![
            ("10925 0.66:VROOT1 --> S1 $.1 [[[false, true]]]", (0.66, "VROOT1".to_owned(), Rhs::Binary("S1".to_owned(), "$.1".to_owned()), vec![vec![false, true]])),
            ("1 6.047046018020197E-5:VROOT1 --> ADV1 $(1 [[[false, true]]]", (6.047046018020197E-5, "VROOT1".to_owned(), Rhs::Binary("ADV1".to_owned(), "$(1".to_owned()), vec![vec![false, true]])),
            ("2 0.006557377049180328:PP2 --> @^PP2^VP3-APPRART1X2  [[[false], [false]]]", (0.006557377049180328, "PP2".to_owned(), Rhs::Chain("@^PP2^VP3-APPRART1X2".to_owned()), vec![vec![false], vec![false]]))
        ];
        for (s, deriv) in ds {
            eprintln!("{:?}", parse_rparse_clauses::<String, f64>(s));
            assert_eq!( (parse_rparse_clauses(s).unwrap().1).0, vec![deriv] );
        }
    }
}