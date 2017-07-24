use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;
use std::collections::HashMap;
use nom::{IResult, is_space};

use util::parsing::*;
use util::integeriser::*;
use approximation::*;

pub struct EquivalenceSet<A, B>{
    pub key: B,
    pub set: Vec<A>,
}

///A struct containing a remapping of Nonterminals into their respective Equivalence classes.
#[derive(Clone)]
pub struct EquivalenceClass<A, B>{
    pub map: HashMap<A, B>,
    pub default: A
}

impl<A: Clone + Eq + Hash, B: Clone + Eq + Hash> EquivalenceClass<A, B> {
    pub fn new(map: HashMap<B, Vec<A>>, default: A) -> EquivalenceClass<A, B> {
        let mut rmap = HashMap::new();
        for (key, set) in map{
            for value in set{
                if rmap.contains_key(&value) {
                    println!("A value happens more than once");
                }else{
                    rmap.insert(value, key.clone());
                }
            }
        }
        if !rmap.contains_key(&default){
            println!("No default element in map");
        }
        EquivalenceClass {
            map: rmap,
            default: default,
        }
    }

    //returns the equivalence class of a given value
    pub fn project(&self, key: A)-> &B{
        if self.map.contains_key(&key){
            self.map.get(&key).unwrap()
        }else{
            self.map.get(&self.default).unwrap()
        }
    }
}

impl <A: Clone + Eq + Hash + FromStr, B: Clone + Eq + Hash + FromStr> FromStr for EquivalenceClass<A, B>
    where <A as FromStr>::Err: Debug, <B as FromStr>::Err: Debug
{
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut map = HashMap::new();
        let mut default=match parse_token(s.as_bytes()){
            IResult::Done(_, result) => result,
            _                        => {return Err(format!("Could not parse {}", s));},
        };
        for l in s.lines() {
            if !l.is_empty(){
                let a: EquivalenceSet<A, B>= try!(l.trim().parse());
                if !map.contains_key(&a.key) {
                    map.insert(a.key.clone(), a.set.clone());
                }
                default=map.get(&a.key).unwrap().last().unwrap().clone();

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

//fits the equivalenz labels for integerise
pub fn in_fit<N: Relabel<A, B, N2> + Hash + Eq + Clone + Ord, N2: Hash + Eq + Clone + Ord, A, B>(eq: EquivalenceClass<A, B>, inter: &Integeriser<N>)-> (EquivalenceClass<u64, u64>, Integeriser<N2>){
    let mut i2 = Integeriser::new();
    let mut nmap = HashMap::new();
    let keys = inter.values();
    for k in keys{
        nmap.insert(*inter.find_key(k.clone()).unwrap(), i2.integerise(k.relabel(&eq)));
    }
    let e = EquivalenceClass{
        map: nmap,
        default: 0 as u64,
    };
    (e, i2)
}
