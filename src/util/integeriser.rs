use std::hash::Hash;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

pub struct Integeriser<A> {
    map: HashMap<u32, A>,
    rmap: HashMap<A, u32>,
    size: u32
}

impl<A: Eq + Hash + Clone> Integeriser<A> {
    pub fn new() -> Integeriser<A> {
        Integeriser { map: HashMap::new(), rmap: HashMap::new(), size: 0 }
    }

    pub fn get_key(&mut self, a: A) -> u32 {
        match self.rmap.entry(a) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                e.insert(self.size);
                self.size += 1;
                self.size - 1
            }
        }
    }

    pub fn find_value(&self, k: u32) -> Option<&A> {
        self.map.get(&k)
    }

    pub fn get_size(&self) -> u32 {
        self.size
    }
}
