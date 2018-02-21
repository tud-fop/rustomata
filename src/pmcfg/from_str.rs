use std::fmt::Debug;
use num_traits::One;
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
                        => return Err(format!("Malformed declaration of initial nonterminals: {}", l))
                }
            },
            _ => return Err("Given string is empty.".to_string())
        }

        for l in s.lines() {
            if !l.is_empty() && !l.starts_with("initial: ") && !l.trim_left().starts_with("%") {
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
          W: FromStr + One,
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

// TODO move this to integration tests
#[test]
fn test_from_str_pmcfg() {
    use log_domain::LogDomain;
    use tree_stack_automaton::TreeStackAutomaton;
    use recognisable::Recognisable;

    let c0: Composition<String> = Composition {
        composition: vec![vec![VarT::Var(0, 0), VarT::Var(1, 0), VarT::Var(0, 1), VarT::Var(1, 1)]],
    };

    let c1: Composition<String> = Composition { composition: vec![vec![], vec![]] };

    let c2 = Composition {
        composition: vec![vec![VarT::T("a".to_string()),
                               VarT::Var(0, 0)],
                          vec![VarT::T("c".to_string()),
                               VarT::Var(0, 1)]],
    };

    let c3 = Composition {
        composition: vec![vec![VarT::T("b".to_string()),
                               VarT::Var(0, 0)],
                          vec![VarT::T("d".to_string()),
                               VarT::Var(0, 1)]],
    };

    let r0: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "S".to_string(),
        tail: vec!["A".to_string(), "B".to_string()],
        composition: c0.clone(),
        weight: LogDomain::new(1.0).unwrap(),
    };

    let r1: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "A".to_string(),
        tail: Vec::new(),
        composition: c1.clone(),
        weight: LogDomain::new(0.6).unwrap(),
    };

    let r2: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "A".to_string(),
        tail: vec!["A".to_string()],
        composition: c2.clone(),
        weight: LogDomain::new(0.4).unwrap(),
    };

    let r3: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "B".to_string(),
        tail: Vec::new(),
        composition: c1.clone(),
        weight: LogDomain::new(0.7).unwrap(),
    };

    let r4: PMCFGRule<String, String, LogDomain<f64>> = PMCFGRule {
        head: "B".to_string(),
        tail: vec!["B".to_string()],
        composition: c3.clone(),
        weight: LogDomain::new(0.3).unwrap(),
    };

    let r0_string = "\"S\" → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (\"A\", B)";
    let r1_string = "A → [[],[]] ()  # 0.6 % this is a comment";
    let r2_string = "A → [[T a, Var 0 0],[T c, Var 0 1]] (A)  # 0.4";
    let r3_string = "B → [[],[]] ()  # 0.7";
    let r4_string = "B → [[T b, Var 0 0],[T d, Var 0 1]] (B)  # 0.3";

    assert_eq!(Ok(r0.clone()),
               r0_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r1.clone()),
               r1_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r2.clone()),
               r2_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r3.clone()),
               r3_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r4.clone()),
               r4_string.parse::<PMCFGRule<String, String, LogDomain<f64>>>());

    let g: PMCFG<String, String, LogDomain<f64>> = PMCFG {
        _dummy: PhantomData,
        initial: vec!["S".to_string()],
        rules: vec![r0.clone(), r1.clone(), r2.clone(), r3.clone(), r4.clone()],
    };

    let mut g_string = String::from("initial: [S]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r3_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r4_string.clone());

    assert_eq!(Ok(g.clone()), g_string.parse());

    let a = TreeStackAutomaton::from(g);

    assert_ne!(None, a.recognise(vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string()]).next());
}
