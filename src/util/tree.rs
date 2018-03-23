use std::collections::BTreeMap;
use std::collections::btree_map;

/// A tree map where each node has a _Gorn address_, i.e. a sequence of integers that describes the
/// path one has to follow to get from the root to the particular node.
#[derive(Debug, Eq)]
pub struct GornTree<V> {
    map: BTreeMap<Vec<usize>, V>,
}

impl<V> GornTree<V> {
    pub fn new() -> GornTree<V> {
        GornTree { map: BTreeMap::new() }
    }

    pub fn clear(&mut self) {
        self.map.clear()
    }

    pub fn get(&self, key: &Vec<usize>) -> Option<&V> {
        self.map.get(key)
    }

    pub fn contains_key(&self, key: &Vec<usize>) -> bool {
        self.map.contains_key(key)
    }

    pub fn get_mut(&mut self, key: &Vec<usize>) -> Option<&mut V> {
        self.map.get_mut(key)
    }

    pub fn insert(&mut self, key: Vec<usize>, value: V) -> Option<V> {
        self.map.insert(key, value)
    }

    pub fn remove(&mut self, key: &Vec<usize>) -> Option<V> {
        self.map.remove(key)
    }

    pub fn append(&mut self, other: &mut GornTree<V>) {
        self.map.append(&mut other.map)
    }

    pub fn entry(&mut self, key: Vec<usize>) -> btree_map::Entry<Vec<usize>, V> {
        self.map.entry(key)
    }

    pub fn split_off(&mut self, key: &Vec<usize>) -> GornTree<V> {
        GornTree { map: self.map.split_off(key) }
    }

    pub fn keys<'a>(&'a self) -> btree_map::Keys<'a, Vec<usize>, V> {
        self.map.keys()
    }

    pub fn values<'a>(&'a self) -> btree_map::Values<'a, Vec<usize>, V> {
        self.map.values()
    }

    pub fn values_mut(&mut self) -> btree_map::ValuesMut<Vec<usize>, V> {
        self.map.values_mut()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<V> IntoIterator for GornTree<V> {
    type Item = (Vec<usize>, V);
    type IntoIter = btree_map::IntoIter<Vec<usize>, V>;

    fn into_iter(self) -> btree_map::IntoIter<Vec<usize>, V> {
        self.map.into_iter()
    }
}

impl<'a, V> IntoIterator for &'a GornTree<V> {
    type Item = (&'a Vec<usize>, &'a V);
    type IntoIter = btree_map::Iter<'a, Vec<usize>, V>;

    fn into_iter(self) -> btree_map::Iter<'a, Vec<usize>, V> {
        (&self.map).into_iter()
    }
}

impl<'a, V> IntoIterator for &'a mut GornTree<V> {
    type Item = (&'a Vec<usize>, &'a mut V);
    type IntoIter = btree_map::IterMut<'a, Vec<usize>, V>;

    fn into_iter(self) -> btree_map::IterMut<'a, Vec<usize>, V> {
        (&mut self.map).into_iter()
    }
}

impl<V: PartialEq> PartialEq for GornTree<V> {
    fn eq(&self, other: &GornTree<V>) -> bool {
        self.map.eq(&other.map)
    }
}
