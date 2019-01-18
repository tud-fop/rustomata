use nom::*;
use std::str::FromStr;
use std::fmt::Debug;
use super::*;

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
                        |s| s == "true"
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
        RparseClauses
    )
}

impl<N, W> FromStr for RparseClauses<N, W>
where 
    N: FromStr,
    W: FromStr,
    <N as FromStr>::Err: Debug,
    <W as FromStr>::Err: Debug,
{
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_rparse_clauses(s) {
            IResult::Done(_, clauses) => Ok(clauses),
            _ => Err(())
        }
    }
}

impl<N, W> Into<Lcfrs<N, (), W>> for RparseClauses<N, W>
where
    N: FromStr,
    <N as FromStr>::Err: Debug
{
    fn into(self) -> Lcfrs<N, (), W> {
        let mut rules = Vec::with_capacity(self.0.len());
        for (weight, lhs, rhs, y) in self.0 {
            let tail = match rhs {
                Rhs::Chain(a) => vec![a],
                Rhs::Binary(a, b) => vec![a, b]
            };

            let mut composition = Vec::with_capacity(y.len());
            let mut ind1 = 0;
            let mut ind2 = 0;
            for ycomp in y {
                let mut component = Vec::with_capacity(ycomp.len());
                for snd_or_fst in ycomp {
                    if snd_or_fst { component.push(VarT::Var(1, ind2)); ind2 += 1; }
                    else { component.push(VarT::Var(0, ind1)); ind1 += 1; }
                }
                composition.push(component);
            }

            rules.push(
                PMCFGRule{ head: lhs, tail, weight, composition: composition.into() }
            );
        }

        Lcfrs{ rules, init: "VROOT1".parse().unwrap() }
    }
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
            assert_eq!( (parse_rparse_clauses(s).unwrap().1).0, vec![deriv] );
        }
    }

    #[test]
    fn conversion () {
        let clauses = vec![
            (0.66, "VROOT1".to_owned(), Rhs::Binary("S1".to_owned(), "$.1".to_owned()), vec![vec![false, true]]),
            (6.047046018020197E-5, "VROOT1".to_owned(), Rhs::Binary("ADV1".to_owned(), "$(1".to_owned()), vec![vec![false, true]]),
            (0.006557377049180328, "PP2".to_owned(), Rhs::Chain("@^PP2^VP3-APPRART1X2".to_owned()), vec![vec![false], vec![false]])
        ];
        let rules: Vec<PMCFGRule<String, (), f64>> = vec![
            PMCFGRule{ head: "VROOT1".to_owned(), tail: vec!["S1".to_owned(), "$.1".to_owned()], weight: 0.66, composition: vec![vec![VarT::Var(0,0), VarT::Var(1,0)]].into() },
            PMCFGRule{ head: "VROOT1".to_owned(), tail: vec!["ADV1".to_owned(), "$(1".to_owned()], weight: 6.047046018020197E-5, composition: vec![vec![VarT::Var(0,0), VarT::Var(1,0)]].into() },
            PMCFGRule{ head: "PP2".to_owned(), tail: vec!["@^PP2^VP3-APPRART1X2".to_owned()], weight: 0.006557377049180328, composition: vec![vec![VarT::Var(0,0)], vec![VarT::Var(0,1)]].into() },
        ];
        
        let conv: Lcfrs<String, (), f64> = RparseClauses(clauses).into();
        assert_eq!(conv.rules, rules);
        assert_eq!(conv.init, "VROOT1".to_owned());
    }
}