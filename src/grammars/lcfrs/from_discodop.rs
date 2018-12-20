use super::*;
use nom::*;
use std::ops::{Div, DivAssign, AddAssign};
use std::str::FromStr;
use std::fmt::Debug;
use std::collections::{hash_map::Entry};
use fnv::FnvHashMap;
use num_traits::One;

#[derive(Debug, PartialEq)]
enum DiscoDeriv<N> {
    Chain{ lhs: N, rhs: N },
    Binary{ lhs: N, rhs1: N, rhs2: N }
}
type DiscoYield = Vec<(u8, u64)>;
#[derive(Debug)]
pub struct DiscoConstituents<N, W>(Vec<(DiscoDeriv<N>, DiscoYield, W)>);
#[derive(Debug)]
pub struct DiscoLexer<N, T, W>(Vec<(T, N, W)>);

#[derive(Debug)]
pub struct DiscoDopGrammar<N, T, W> {
    pub constituents: DiscoConstituents<N, W>,
    pub lexer: Option<DiscoLexer<N, T, W>>
}

fn is_token(c: char) -> bool { !c.is_whitespace() }
fn is_binary_digit(c: char) -> bool { c == '0' || c == '1' }

/// Interprets a string over { '0', '1' } in reverse as an integer in binary format.
fn from_binary(s: &str) -> Option<u64> {
    s.chars()
     .enumerate()
     .map(|(i,c)| match c { '0' => Some(0), '1' => Some(2u64.pow(i as u32)), _ => None })
     .fold(Some(0), |os, ov| os.and_then(|s| ov.and_then(|v| Some(v + s))))
}

impl<N> DiscoDeriv<N> {
    fn get_lhs(&self) -> &N {
        match self {
            &DiscoDeriv::Chain{ ref lhs, .. } => lhs,
            &DiscoDeriv::Binary{ ref lhs, .. } => lhs
        }
    }
}

/// Parses a yield function given in binary format.
/// Assuming a binary, sorted lcfrs rule, each consecutive 0 is a new variable of the first
/// successor, and each 1 is a new variable for the second successor. We represent this
/// for each component by the length of the composition component and a bitmask.
named!(parse_yield<&str,DiscoYield>,
    separated_nonempty_list_complete!(
        tag!(","),
        map_opt!(
            take_while!(is_binary_digit),
            |bs: &str| -> Option<(u8, u64)> {
                let payload = from_binary(bs)?;
                Some((bs.len() as u8, payload))
            }
        )
    )
);

/// Parse the weight given in the disco-dop format.
/// Weights are given as pseudocounts, which are either fractions (numerator/denominator), or
/// integer values.
fn parse_pseudocount<W>(s: &str) -> IResult<&str, W>
where
    W: Div<Output=W> + FromStr,
{
    map_res!(
        s,
        tuple!(digit, opt!(complete!(preceded!(tag!("/"), digit)))),
        |(num,denom): (&str, Option<&str>)| -> Result<W, W::Err> {
            if let Some(denominator) = denom {
                let numerator: W = num.parse()?;
                let denominator: W = denominator.parse()?;
                Ok(numerator / denominator)
            } else {
                num.parse()
            }
        }
    )
}

#[derive(Debug, Clone, Copy)]
pub enum ParseLexerError {
    Lhs, Rhs
}
/// Parse a lexer file.
/// Word-POS-weight-pairs are given in a list of POS-weight pairs for a word in each line.
fn parse_discodop_lexer<N, T, W>(s: &str) -> IResult<&str, DiscoLexer<N, T, W>>
where
    N: FromStr,
    T: FromStr,
    W: Div<Output=W> + FromStr,
{
    map_res!(
        s,
        separated_list_complete!(
            tag!("\n"),
            tuple!(
                terminated!(take_while!(is_token), space),
                separated_nonempty_list_complete!(
                    space,
                    tuple!(
                        terminated!(take_while!(is_token), space),
                        parse_pseudocount
                    )
                )
            )
        ),
        |v: Vec<(&str, Vec<(&str, W)>)>| -> Result<DiscoLexer<N, T, W>, ParseLexerError> {
            let mut lexer = Vec::with_capacity(v.iter().map(|(_, pws)| pws.len()).sum());
            for (word, pws) in v {
                for (pos, weight) in pws {
                    let w = word.parse().map_err(|_| ParseLexerError::Rhs)?;
                    let p = pos.parse().map_err(|_| ParseLexerError::Lhs)?;
                    lexer.push((w, p, weight));
                }
            }
            Ok(DiscoLexer(lexer))
        }
    )
}

#[derive(Debug,Clone,Copy)]
pub enum ParseConsituentsError {
    Lhs, Rhs, Weight, Yield
}
/// Parse a list of Constituent rules given in disco-dop's format.
/// Each line contains a rule with tab-separaterd values for:
/// * lhs nonterminal,
/// * rhs nonterminal, and optional a second rhs nonterminal,
/// * yield function, and
/// * weight.
fn parse_discodop_constituents<N, W>(s: &str) -> IResult<&str, DiscoConstituents<N, W>>
where
    N: FromStr,
    W: Div<Output=W> + FromStr,
{
    map!(
        s,
        separated_list_complete!(
            tag!("\n"),
            map_res!(
                tuple!(
                    take_while1!(is_token),
                    count_fixed!(&str, preceded!(space, take_while1!(is_token)), 3),
                    opt!(complete!(preceded!(space, take_while1!(is_token))))
                ), 
                |(s_lhs, [s_rhs, s_rhs_or_y, s_y_or_w], s_ow): (&str, [&str;3], Option<&str>)| -> Result<(DiscoDeriv<N>, DiscoYield, W), ParseConsituentsError> {
                    let lhs = s_lhs.parse().map_err(|_| ParseConsituentsError::Lhs)?;
                    let rhs = s_rhs.parse().map_err(|_| ParseConsituentsError::Rhs)?;
                    let (deriv, y, w) = if let Some(s_weight) = s_ow {
                        let rhs2 = s_rhs_or_y.parse().map_err(|_| ParseConsituentsError::Rhs)?;
                        (DiscoDeriv::Binary{lhs, rhs1: rhs, rhs2}, s_y_or_w, s_weight)
                    } else {
                        (DiscoDeriv::Chain{lhs, rhs}, s_rhs_or_y, s_y_or_w)
                    };
                    let ys = parse_yield(y).to_result().map_err(|_| ParseConsituentsError::Yield)?;
                    let weight = parse_pseudocount(w).to_result().map_err(|_| ParseConsituentsError::Weight)?;
                    Ok((deriv, ys, weight))
                }
            )
        ),
        |v| DiscoConstituents(v)
    )
}


impl<N, T, W> From<DiscoDopGrammar<N, T, W>> for Lcfrs<N, T, W>
where
    W: DivAssign + Copy + AddAssign,
    N: FromStr + Hash + Eq + Clone,
    <N as FromStr>::Err: Debug,
{
    fn from(gmr: DiscoDopGrammar<N, T, W>) -> Self {
        let mut rules = Vec::with_capacity(gmr.constituents.0.len() + gmr.lexer.as_ref().map_or(0, |l| l.0.len()));

        let mut normalization_denominators: FnvHashMap<N, W> = HashMap::default();
        for (lhs, weight) in gmr.constituents.0.iter().map(|&(ref d, _, w)| (d.get_lhs(), w))
                                .chain(gmr.lexer.iter().flat_map(|l| &l.0).map(|&(_, ref lhs, w)| (lhs, w))) {
            match normalization_denominators.entry(lhs.clone()) {
                Entry::Occupied(mut oe) => { *oe.get_mut() += weight; },
                Entry::Vacant(ve) => { ve.insert(weight); }
            }
        }

        for (nts, y, mut weight) in gmr.constituents.0 {
            let mut composition: Vec<Vec<VarT<T>>> = Vec::with_capacity(y.len());
            let mut sind1: usize = 0;
            let mut sind2: usize = 0;
            weight /= *normalization_denominators.get(nts.get_lhs()).unwrap();

            // read yield function
            for (clen, c) in y {
                let mut component = Vec::with_capacity(clen as usize);
                for i in 0..clen {
                    if 2u64.pow(i as u32) & c == 0 { component.push(VarT::Var(0, sind1)); sind1 += 1; }
                    else { component.push(VarT::Var(1, sind2)); sind2 += 1; }
                }
                composition.push(component);
            }

            // compose rules using yield function and nonterminals
            match nts {
                DiscoDeriv::Chain{ lhs, rhs } => {
                    rules.push(PMCFGRule{ head: lhs, tail: vec![rhs], weight: weight, composition: composition.into() });
                }
                DiscoDeriv::Binary{ lhs, rhs1, rhs2 } => {
                    rules.push(PMCFGRule{ head: lhs, tail: vec![rhs1, rhs2], weight, composition: composition.into() });
                }
            }
        }

        for (word, pos, mut weight) in gmr.lexer.into_iter().flat_map(|dl| dl.0) {
            weight /= *normalization_denominators.get(&pos).unwrap();
            rules.push(PMCFGRule{ head: pos, tail: vec![], weight, composition: vec![vec![VarT::T(word)]].into() })
        }

        Lcfrs{ rules, init: "ROOT".parse::<N>().unwrap() }
    }
}

impl<N, W> DiscoDopGrammar<N, (), W> {
    pub fn with_default_lexer(self) -> DiscoDopGrammar<N, N, W>
    where
        N: Clone + Hash + Eq,
        W: One
    {
        let mut lex = Vec::new();
        
        {
            let mut is_only_on_rhs: FnvHashMap<&N, bool> = HashMap::default();
            for &(ref deriv, _, _) in &self.constituents.0 {
                match deriv {
                    &DiscoDeriv::Chain{ ref lhs, ref rhs } => {
                        *is_only_on_rhs.entry(lhs).or_insert(false) = false;
                        is_only_on_rhs.entry(rhs).or_insert(true);
                    },
                    &DiscoDeriv::Binary{ ref lhs, ref rhs1, ref rhs2 } => {
                        *is_only_on_rhs.entry(lhs).or_insert(false) = false;
                        is_only_on_rhs.entry(rhs1).or_insert(true);
                        is_only_on_rhs.entry(rhs2).or_insert(true);
                    }
                }
            }
            
            for p_pos in is_only_on_rhs.into_iter().filter_map(|(v, b)| if b { Some(v) } else { None }) {
                lex.push(((*p_pos).clone(), (*p_pos).clone(), W::one()));
            }
        }

        DiscoDopGrammar {
            constituents: self.constituents,
            lexer: Some(DiscoLexer(lex))
        }
    }

    pub fn with_lexer<T>(self, lexer: DiscoLexer<N, T, W>) -> DiscoDopGrammar<N, T, W> {
        DiscoDopGrammar{ constituents: self.constituents, lexer: Some(lexer) }
    }
}

impl<N, W> FromStr for DiscoDopGrammar<N, (), W>
where
    N: FromStr,
    W: Div<Output=W> + FromStr,
    <W as FromStr>::Err: Debug,
    <N as FromStr>::Err: Debug,
{
    type Err = nom::Err<u32>;
    fn from_str(constituent_str: &str) -> Result<Self, Self::Err>
    {
        Ok(DiscoDopGrammar{
            constituents: parse_discodop_constituents(constituent_str).to_result()?,
            lexer: None
        })
    }
}

impl<N, T, W> FromStr for DiscoLexer<N, T, W>
where
    N: FromStr,
    T: FromStr,
    W: Div<Output=W> + FromStr,
    <W as FromStr>::Err: Debug,
    <N as FromStr>::Err: Debug,
    <T as FromStr>::Err: Debug,
{
    type Err = nom::Err<u32>;
    fn from_str(lexer_str: &str) -> Result<Self, Self::Err> {
        parse_discodop_lexer(lexer_str).to_result()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn weight () {
        let ws = vec![
            ("1", 1f64),
            ("1000", 1000f64),
            ("23/64", 23f64 / 64f64)
        ];
        for (s, w) in ws {
            assert_eq!(parse_pseudocount::<f64>(s).unwrap().1, w);
        }
    }

    #[test]
    fn lexer () {
        let lex = parse_discodop_lexer(&"is\tVB\t1/2\nJohn\tNN\t1\nrich\tJJ\t1/4\nstain\tNN\t2/8\tVP\t3");
        assert_eq!(
            (lex.unwrap().1).0, 
            vec![ ("is".to_string(), "VB".to_string(), 0.5f64),
                  ("John".to_string(), "NN".to_string(), 1f64),
                  ("rich".to_string(), "JJ".to_string(), 0.25f64),
                  ("stain".to_string(), "NN".to_string(), 0.25f64),
                  ("stain".to_string(), "VP".to_string(), 3f64) ]
        )
    }

    #[test]
    fn yield_binary () {
        let ys = vec![
            ("000", Some(0b0)),
            ("10111010", Some(0b01011101)),
            ("", Some(0b0)),
            ("2", None)
        ];
        for (y, yp) in ys {
            assert_eq!(from_binary(y), yp);
        }
    }

    #[test]
    fn yield_repr () {
        let ys = vec![
            ("000,11111", vec![(3, 0b000), (5, 0b11111)]),
            ("0101010,10111010", vec![(7, 0b0101010), (8, 0b01011101)]),
            ("0", vec![(1, 0b0)]),
            ("1,0,1,0,10100,01,0,1", vec![(1, 0b1),(1, 0),(1, 0b1),(1, 0b0),(5, 0b00101),(2, 0b10),(1, 0b0),(1, 0b1)]),
        ];

        for (y, yv) in ys {
            let ryv = parse_yield(y);
            assert_eq!(ryv.unwrap().1, yv);
        }
    }

    #[test]
    fn const_rule () {
        let ds = vec![
            ("S\tNP\tVP\t01,10\t5", (DiscoDeriv::Binary{ lhs: "S".to_string(), rhs1: "NP".to_string(), rhs2: "VP".to_string() }, vec![(2,0b10),(2,0b01)], 5f64)),
            ("S\tA\t0\t1", (DiscoDeriv::Chain{ lhs: "S".to_string(), rhs: "A".to_string() }, vec![(1,0b0)], 1f64)),
            ("NP_1\tDET_2\tNP_55\t0001,11110,001010,0010100\t5", (DiscoDeriv::Binary{ lhs: "NP_1".to_string(), rhs1: "DET_2".to_string(), rhs2: "NP_55".to_string() }, vec![(4,0b1000),(5,0b01111),(6,0b010100),(7,0b0010100)], 5f64)),
            ("NP|<DET>\tNP\t0110\t64", (DiscoDeriv::Chain{ lhs:"NP|<DET>".to_string(), rhs: "NP".to_string() }, vec![(4,0b0110)], 64f64)),
        ];
        for (s, deriv) in ds {
            assert_eq!( (parse_discodop_constituents(s).unwrap().1).0, vec![deriv] );
        }
    }

    #[test]
    fn constituents () {
        let cons = parse_discodop_constituents("S\tNP\tVP\t010\t1\nNP_2\tVP\tNP\t0,1\t2\nNP\tNN\t0\t4");
        assert_eq!(
            (cons.unwrap().1).0,
            vec![
                (DiscoDeriv::Binary{ lhs: "S".to_string(), rhs1: "NP".to_string(), rhs2: "VP".to_string() }, vec![(3,0b010)], 1f64),
                (DiscoDeriv::Binary{ lhs: "NP_2".to_string(), rhs1: "VP".to_string(), rhs2: "NP".to_string()}, vec![(1u8,0b0u64),(1,0b1)], 2f64),
                (DiscoDeriv::Chain{ lhs: "NP".to_string(), rhs: "NN".to_string() }, vec![(1,0b0)], 4f64),
            ]
        )
    }

    #[test]
    fn conversion () {
        let dlex = vec![("a", "DET".to_string(), 5f64),("word", "NP".to_string(), 6f64)];
        let drules = vec![(DiscoDeriv::Binary{ lhs: "S".to_string(), rhs1: "NP".to_string(), rhs2: "VP".to_string() }, vec![(2, 0b10)], 5f64),(DiscoDeriv::Chain{ lhs: "NP".to_string(), rhs: "NP".to_string() }, vec![(2, 0b00)], 5f64)];

        let grmr: Lcfrs<String, &str, f64> = DiscoDopGrammar{
            lexer: Some(DiscoLexer(dlex)),
            constituents: DiscoConstituents(drules)
        }.into();
        assert_eq!(grmr.init, "ROOT");
        assert_eq!(grmr.rules,
            vec![
                PMCFGRule{ head: "S".to_string(), tail: vec!["NP".to_string(), "VP".to_string()], composition: vec![vec![VarT::Var(0,0), VarT::Var(1,0)]].into(), weight: 5f64 },
                PMCFGRule{ head: "NP".to_string(), tail: vec!["NP".to_string()], composition: vec![vec![VarT::Var(0,0), VarT::Var(0,1)]].into(), weight: 5f64 },
                PMCFGRule{ head: "DET".to_string(), tail: vec![], composition: vec![vec![VarT::T("a")]].into(), weight: 5f64 },
                PMCFGRule{ head: "NP".to_string(), tail: vec![], composition: vec![vec![VarT::T("word")]].into(), weight: 6f64 },
            ]
        )
    }
}