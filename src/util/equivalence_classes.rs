use std::fmt::Debug;
use std::hash::Hash;
use std::collections::hash_map::Entry;
use std::str::FromStr;
use std::collections::HashMap;
use nom::{IResult, is_space};
use integeriser::{Integeriser, HashIntegeriser};

use util::parsing::*;
use approximation::*;

/// Structure containing the elements of type `A` in a equivalence class of type `B`
pub struct EquivalenceSet<A, B>{
    pub key: B,
    pub set: Vec<A>,
}

/// A struct containing a remapping of elements of type `A` into their respective equivalence classes of type `B`.
#[derive(Clone)]
pub struct EquivalenceClass<A, B>{
    pub map: HashMap<A, B>,
    pub default: B,
}

impl<A: Clone + Eq + Hash, B: Clone + Debug + Eq + Hash> EquivalenceClass<A, B> {
    pub fn new(map: HashMap<B, Vec<A>>, default: B) -> EquivalenceClass<A, B> {
        let mut rmap = HashMap::new();
        for (class_name, members) in map {
            for value in members {
                match rmap.entry(value) {
                    Entry::Vacant(e) => { e.insert(class_name.clone()); },
                    Entry::Occupied(e) => panic!("The value {:?} occurs more than once", e.get()),
                }
            }
        }
        EquivalenceClass {
            map: rmap,
            default: default,
        }
    }

    // returns the equivalence class of a given value
    pub fn project(&self, key: &A)-> &B {
        match self.map.get(key) {
            None => &self.default,
            Some(e) => e,
        }
    }
}

impl <A: Clone + Eq + Hash + FromStr, B: Clone + Debug + Eq + Hash + FromStr> FromStr for EquivalenceClass<A, B>
    where <A as FromStr>::Err: Debug, <B as FromStr>::Err: Debug
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
                let a: EquivalenceSet<A, B> = try!(l.trim().parse());
                if !map.contains_key(&a.key) {
                    map.insert(a.key.clone(), a.set.clone());
                }
                default = a.key.clone();

            }
        }

        Ok(EquivalenceClass::new(map, default))

    }
}

impl <A: FromStr, B: FromStr> FromStr for EquivalenceSet<A, B>
    where <A as FromStr>::Err: Debug, <B as FromStr>::Err: Debug
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_set(s.as_bytes()) {
            IResult::Done(_, result) => Ok(result),
            _                        => Err(format!("Could not parse {}", s))
        }
    }
}

fn parse_set<A: FromStr, B: FromStr>(input: &[u8]) -> IResult<&[u8], EquivalenceSet<A, B>>
    where <A as FromStr>::Err: Debug, <B as FromStr>::Err: Debug
{
    do_parse!(
        input,
        key: parse_token >>
            take_while!(is_space) >>
        set: parse_heap >>
            (EquivalenceSet {
                key: key,
                set: set,
            })
    )
}

fn parse_heap<A:FromStr>(input: &[u8]) -> IResult<&[u8], Vec<A>>
    where <A as FromStr>::Err: Debug
{
    parse_vec(input, parse_token, "[", "]", ",")
}

// fits the equivalence labels for integerise
pub fn in_fit<N: Relabel<A, B, N2> + Hash + Eq + Clone + Ord, N2: Hash + Eq + Clone + Ord, A, B>(eq: &EquivalenceClass<A, B>, inter: &HashIntegeriser<N>) -> (EquivalenceClass<usize, usize>, HashIntegeriser<N2>) {
    let mut i2 = HashIntegeriser::new();
    let mut nmap = HashMap::new();
    let keys = inter.values();
    for k in keys {
        nmap.insert(inter.find_key(k).unwrap(), i2.integerise(k.relabel(eq)));
    }
    let e = EquivalenceClass {
        map: nmap,
        default: 0usize,
    };
    (e, i2)
}
