use std::fmt::Debug;
use std::str::{FromStr};

use nom::{IResult};

use util::parsing::*;
use super::*;

impl<N, T, W> FromStr for Lcfrs<N, T, W>
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
        let mut initial;

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
        
        Ok(Lcfrs {
            init: initial.remove(0),
            rules: rules,
        })

    }
}
