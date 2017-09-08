use std::hash::Hash;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

/// Structure that maps to every element of type `A` a integer of type `u64`. Mapping goes both ways.
#[derive(Clone, Debug)]
pub struct Integeriser<A: Hash + Eq> {
    pub map: HashMap<u64, A>,
    pub rmap: HashMap<A, u64>,
    size: u64
}

impl<A: Eq + Hash + Clone> Integeriser<A> {
    pub fn new() -> Integeriser<A> {
        Integeriser { map: HashMap::new(), rmap: HashMap::new(), size: 0 }
    }

    pub fn integerise(&mut self, a: A) -> u64 {
        match self.rmap.entry(a) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                self.map.insert(self.size, e.key().clone());
                e.insert(self.size);
                self.size += 1;
                self.size - 1
            }
        }
    }

    pub fn find_value(&self, k: u64) -> Option<&A> {
        self.map.get(&k)
    }

    pub fn find_key(&self, v: A) -> Option<&u64> {
        self.rmap.get(&v)
    }

    pub fn get_size(&self) -> u64 {
        self.size
    }

    pub fn values(&self) -> Vec<A>{
        let mut v = Vec::new();
        for k in self.rmap.keys(){
            v.push(k.clone());
        }
        v
    }
}
