use std::fmt::Debug;
use std::hash::Hash;
use std::collections::hash_map::Entry;
use std::str::FromStr;
use std::collections::HashMap;
use nom::{IResult, is_space};

use util::parsing::*;

/// Structure containing the elements of type `A` in a equivalence class of type `B`
pub struct EquivalenceClass<A, B>{
    pub key: B,
    pub set: Option<Vec<A>>,
}

/// A struct containing a remapping of elements of type `A` into their respective equivalence classes of type `B`.
#[derive(Clone, Debug)]
pub struct EquivalenceRelation<A, B>
    where A: Eq + Hash,
{
    pub map: HashMap<A, B>,
    pub default: B,
}

impl<A, B> PartialEq for EquivalenceRelation<A, B>
    where A: Eq + Hash,
          B: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.default == other.default
            && self.map.keys().all(|k| self.map[k] == other.map[k])
            && other.map.keys().all(|k| self.map[k] == other.map[k])
    }
}

impl<A: Eq + Hash, B: Clone + Eq + Hash> EquivalenceRelation<A, B> {
    pub fn new(map: HashMap<B, Vec<A>>, default: B) -> Self {
        let mut rmap = HashMap::new();
        for (class_name, members) in map {
            for value in members {
                match rmap.entry(value) {
                    Entry::Vacant(v) => { v.insert(class_name.clone()); },
                    Entry::Occupied(_) => panic!("The value occurs more than once in the equivalence class"),
                }
            }
        }
        EquivalenceRelation {
            map: rmap,
            default: default,
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

impl <A, B> FromStr for EquivalenceRelation<A, B>
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
                    EquivalenceClass { key, set: Some(set) } => { map.insert(key, set); },
                    EquivalenceClass { key, set: None } => { default = key; },
                }
            }
        }

        Ok(EquivalenceRelation::new(map, default))

    }
}

impl <A, B> FromStr for EquivalenceClass<A, B>
    where A: FromStr,
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
    where A: FromStr,
          A::Err: Debug,
          B: FromStr,
          B::Err: Debug,
{
    do_parse!(
        input,
        key: parse_token >>
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
                key: key,
                set: set,
            })
    )
}

fn parse_heap<A>(input: &[u8]) -> IResult<&[u8], Vec<A>>
    where A: FromStr,
          A::Err: Debug,
{
    parse_vec(input, parse_token, "[", "]", ",")
}

#[test]
fn test_equivalence_relation() {

    let rel_parse: EquivalenceRelation<usize, usize> = "0 [0, 1]\n1 [2, 3]\n3 *".to_string().parse().unwrap();
    let mut map = HashMap::new();
    map.insert(0usize, vec![0usize, 1usize]);
    map.insert(1usize, vec![2usize, 3usize]);
    let rel = EquivalenceRelation::new(map, 3usize);

    assert_eq!(rel_parse, rel);
}
