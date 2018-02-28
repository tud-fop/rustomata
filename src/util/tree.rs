use std::collections::BTreeMap;
use std::collections::btree_map;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Eq)]
pub struct GornTree<V> {
    map: BTreeMap<Vec<usize>, V>,
}

impl<V> GornTree<V> {
    pub fn new() -> GornTree<V> {
        GornTree { map: BTreeMap::new() }
    }
}

impl<V> Deref for GornTree<V> {
    type Target = BTreeMap<Vec<usize>, V>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl<V> DerefMut for GornTree<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
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
