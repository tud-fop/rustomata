use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::{FromStr, from_utf8};

use nom::{IResult, is_space};
use num_traits::One;

use cfg::{LetterT, CFGComposition, CFGRule, CFG};
use util::parsing::*;

impl<N, T, W> FromStr for CFG<N, T, W>
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
          W: FromStr + One,
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
            (CFGRule {
                head: head,
                composition: CFGComposition { composition: composition },
                weight: weight_o.unwrap_or(W::one()),
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


// TODO move this to integration tests
#[test]
fn test_from_str_cfg() {
    use log_domain::LogDomain;
    use push_down_automaton::PushDownAutomaton;
    use recognisable::Recognisable;

    let c0 : CFGComposition<String,String> = CFGComposition {
        composition: vec![LetterT::Label("A".to_string())],
    };

    let c1 : CFGComposition<String,String>  = CFGComposition {
        composition: vec![LetterT::Value("a".to_string()),LetterT::Label("A".to_string()),LetterT::Label("B".to_string())]
    };

    let c2 : CFGComposition<String,String>  = CFGComposition {
        composition: vec![LetterT::Value("a".to_string())],
    };

    let c3 : CFGComposition<String,String>  = CFGComposition {
        composition: vec![LetterT::Value("b".to_string())],
    };

    let r0: CFGRule<String, String, LogDomain<f64>> = CFGRule {
        head: "S".to_string(),
        composition: c0.clone(),
        weight: LogDomain::one(),
    };

    let r1: CFGRule<String, String, LogDomain<f64>> = CFGRule {
        head: "A".to_string(),
        composition: c1.clone(),
        weight: LogDomain::new(0.6).unwrap(),
    };

    let r2: CFGRule<String, String, LogDomain<f64>> = CFGRule {
        head: "A".to_string(),
        composition: c2.clone(),
        weight: LogDomain::new(0.4).unwrap(),
    };

    let r3: CFGRule<String, String, LogDomain<f64>> = CFGRule {
        head: "B".to_string(),
        composition: c3.clone(),
        weight: LogDomain::one(),
    };

    let r0_string = "S → [Nt A]";
    let r1_string = "A → [T a, Nt A, Nt B] # 0.6";
    let r2_string = "A → [T a] # 0.4";
    let r3_string = "B → [T b] # 1";

    assert_eq!(Ok(r0.clone()),
               r0_string.parse::<CFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r1.clone()),
               r1_string.parse::<CFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r2.clone()),
               r2_string.parse::<CFGRule<String, String, LogDomain<f64>>>());
    assert_eq!(Ok(r3.clone()),
               r3_string.parse::<CFGRule<String, String, LogDomain<f64>>>());

    let g: CFG<String, String, LogDomain<f64>> = CFG {
        _dummy: PhantomData,
        initial: vec!["S".to_string(),"B".to_string()],
        rules: vec![r0.clone(), r1.clone(), r2.clone(), r3.clone()],
    };

    let mut g_string = String::from("initial: [S, B]\n\n");
    g_string.push_str(r0_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r1_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r2_string.clone());
    g_string.push_str("\n");
    g_string.push_str(r3_string.clone());

    assert_eq!(Ok(g.clone()), g_string.parse());

    let a = PushDownAutomaton::from(g);

    assert_ne!(None, a.recognise(vec!["a".to_string(), "a".to_string(), "a".to_string(), "b".to_string(), "b".to_string()]).next());
}

