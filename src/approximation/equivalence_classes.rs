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
        let mut rmap = HashMap::new();
        for (class_name, members) in map {
            for value in members {
                match rmap.entry(value) {
                    Entry::Vacant(v) => { v.insert(class_name.clone()); },
                    Entry::Occupied(_) => panic!("The same value occurs in two equivalence classes!"),
                }
            }
        }

        EquivalenceRelation {
            map: rmap,
            default,
        }
    }

    // returns the equivalence class of a given value
    pub fn project(&self, key: &A) -> B {
        match self.map.get(key) {
            None => self.default.clone(),
            Some(e) => e.clone(),
        }
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
