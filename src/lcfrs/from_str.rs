use std::fmt::Debug;
use std::str::FromStr;
use num_traits::One;

use util::parsing::initial_rule_grammar_from_str;
use super::Lcfrs;

impl<N, T, W> FromStr for Lcfrs<N, T, W>
    where N: FromStr,
          N::Err: Debug,
          T: Clone + FromStr,
          T::Err: Debug,
          W: FromStr + One,
          W::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (mut initials, rules) = initial_rule_grammar_from_str(s)?;
        if initials.len() != 1 {
            Err("multiple initial nonterminals".to_owned())
        } else {
            Ok(
                Lcfrs{ init: initials.remove(0),
                       rules
                }
            )
        }
    }
}
