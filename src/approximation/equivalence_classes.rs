use nom::{IResult, is_space};
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::FromIterator;
use std::str::FromStr;

use util::parsing::*;

/// Structure containing the elements of type `A` in a equivalence class of type `B`
#[derive(Debug, Eq, PartialEq)]
pub struct EquivalenceClass<A, B>
    where A: Eq + Hash,
{
    label: B,
    set: Option<HashSet<A>>,
}

impl<A, B> From<(B, Option<Vec<A>>)> for EquivalenceClass<A, B>
    where A: Eq + Hash,
{
    fn from((label, set): (B, Option<Vec<A>>)) -> EquivalenceClass<A, B> {
        EquivalenceClass {
            label,
            set: set.and_then(|elements| Some(HashSet::from_iter(elements))),
        }
    }
}

/// A struct containing a remapping of elements of type `A` into their respective equivalence classes of type `B`.
#[derive(Clone, Debug)]
pub struct EquivalenceRelation<A, B>
    where A: Eq + Hash,
{
    map: HashMap<A, B>,
    default: B,
}

impl<A, B> PartialEq for EquivalenceRelation<A, B>
    where A: Eq + Hash,
          B: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.default == other.default && self.map == other.map
    }
}

impl<A, B> EquivalenceRelation<A, B>
    where A: Eq + Hash,
          B: Clone + Eq + Hash,
{
    pub fn new(map: HashMap<B, HashSet<A>>, default: B) -> Self {
        match Self::new_safe(map, default) {
            Ok(relation) => relation,
            Err(error) => panic!(error),
        }
    }

    pub fn new_safe(map: HashMap<B, HashSet<A>>, default: B) -> Result<Self, String> {
        let mut relation_map = HashMap::new();
        for (class_name, members) in map {
            if class_name == default {
                return Err(String::from(
                    "There can only be one default class in the equivalence relation!"
                ));
            }

            for value in members {
                match relation_map.entry(value) {
                    Entry::Vacant(v) => {
                        v.insert(class_name.clone());
                    },
                    Entry::Occupied(_) => {
                        return Err(String::from(
                            "All classes of the equivalence relation must be disjoint!"
                        ));
                    },
                }
            }
        }

        Ok(EquivalenceRelation { map: relation_map, default })
    }

    // returns the equivalence class of a given value
    pub fn project(&self, key: &A) -> B {
        match self.map.get(key) {
            None => self.default.clone(),
            Some(e) => e.clone(),
        }
    }
}

impl<A, B> From<Vec<EquivalenceClass<A, B>>> for EquivalenceRelation<A, B>
    where A: Eq + Hash,
          B: Clone + Eq + Hash,
{
    fn from(classes: Vec<EquivalenceClass<A, B>>) -> EquivalenceRelation<A, B> {
        let mut map = HashMap::new();
        let mut default = None;

        for EquivalenceClass { label, set } in classes {
            if let Some(elements) = set {
                for element in elements {
                    map.insert(element, label.clone());
                }
            } else {
                default = Some(label);
            }
        }

        EquivalenceRelation { map, default: default.unwrap() }
    }
}

impl<A, B> FromStr for EquivalenceRelation<A, B>
    where A: Clone + Eq + Hash + FromStr,
          A::Err: Debug,
          B: Clone + Eq + Hash + FromStr,
          B::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut map = HashMap::new();
        let mut default = match parse_token(s.as_bytes()) {
            IResult::Done(_, result) => result,
            _                        => return Err(format!("Could not parse {}", s)),
        };
        for l in s.lines() {
            if !l.is_empty() {
                match l.trim().parse()? {
                    EquivalenceClass { label, set: Some(elements) } => {
                        map.insert(label, elements);
                    },
                    EquivalenceClass { label, set: None } => {
                        default = label;
                    },
                }
            }
        }

        Ok(EquivalenceRelation::new(map, default))

    }
}

impl <A, B> FromStr for EquivalenceClass<A, B>
    where A: Eq + FromStr + Hash,
          A::Err: Debug,
          B: FromStr,
          B::Err: Debug,
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_set(s.as_bytes()) {
            IResult::Done(_, result) => Ok(result),
            _                        => Err(format!("Could not parse {}", s))
        }
    }
}

fn parse_set<A, B>(input: &[u8]) -> IResult<&[u8], EquivalenceClass<A, B>>
    where A: Eq + FromStr + Hash,
          A::Err: Debug,
          B: FromStr,
          B::Err: Debug,
{
    do_parse!(
        input,
        label: parse_token >>
            take_while!(is_space) >>
            set: alt!(
                do_parse!(
                    tag!("*") >> (None)
                )
                    |
                do_parse!(
                    the_set: parse_heap >> (Some(the_set))
                )
            ) >>
            (EquivalenceClass {
                label,
                set,
            })
    )
}

fn parse_heap<A>(input: &[u8]) -> IResult<&[u8], HashSet<A>>
    where A: Eq + FromStr + Hash,
          A::Err: Debug,
{
    match parse_vec(input, parse_token, "[", "]", ",") {
        IResult::Done(rest, parsed) => IResult::Done(rest, HashSet::from_iter(parsed)),
        IResult::Incomplete(needed) => IResult::Incomplete(needed),
        IResult::Error(error) => IResult::Error(error),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_equivalence_relation_project() {
        let rel: EquivalenceRelation<u8, u8> = String::from("0 [0, 1]\n1 [2, 4]\n2 *").parse().unwrap();
        assert_eq!(0, rel.project(&1));
        assert_eq!(1, rel.project(&2));
        assert_eq!(2, rel.project(&3));
        assert_eq!(1, rel.project(&4));
    }

    #[test]
    fn test_equivalence_class_from_str_legal_input() {
        let legal_inputs = vec![
            ("0 [0, 1]", EquivalenceClass::from((0, Some(vec![0, 1])))),
            ("0  [1, 0]", EquivalenceClass::from((0, Some(vec![0, 1])))),
            ("0  [0, 1, 1]", EquivalenceClass::from((0, Some(vec![0, 1])))),
            ("0 *", EquivalenceClass::from((0, None))),
        ];

        for (legal_input, correct_class) in legal_inputs {
            assert_eq!(correct_class,
                       EquivalenceClass::from_str(legal_input).unwrap());
        }
    }

    #[test]
    fn test_equivalence_class_from_str_illegal_input() {
        let illegal_inputs = vec![
            " 0 [0, 1]",
            "[0, 1]",
            "*",
        ];

        for illegal_input in illegal_inputs {
            match EquivalenceClass::<u8, u8>::from_str(illegal_input) {
                Ok(parsed) =>
                    panic!("Was able to parse the illegal input \'{}\' as \'{:?}\'",
                           illegal_input, parsed),
                Err(_) => (),
            }
        }
    }

    #[test]
    fn test_equivalence_relation_from_str_legal_input() {
        let legal_inputs = vec![
            ("0 [0, 1]\n1 [2, 3]\n2 *", EquivalenceRelation::from(vec![
                EquivalenceClass::from((0, Some(vec![0, 1]))),
                EquivalenceClass::from((1, Some(vec![2, 3]))),
                EquivalenceClass::from((2, None)),
            ])),
        ];

        for (legal_input, correct_relation) in legal_inputs {
            assert_eq!(correct_relation,
                       EquivalenceRelation::from_str(legal_input).unwrap());
        }
    }

    #[test]
    fn test_equivalence_relation_from_str_illegal_input() {
        let illegal_inputs = vec![
            " 0 [0, 1]\n1 [2, 3]\n2 *",
            "0 [0, 1]1 [2, 3]2 *",
            "0 [0, 1]\n1 [1, 2]\n2 *",
        ];

        for illegal_input in illegal_inputs {
            match EquivalenceRelation::<u8, u8>::from_str(illegal_input) {
                Ok(parsed) =>
                    panic!("Was able to parse the illegal input \'{}\' as \'{:?}\'",
                           illegal_input, parsed),
                Err(_) => (),
            }
        }
    }
}
