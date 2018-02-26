use std::collections::BTreeMap;
use std::collections::btree_map::{IntoIter, Iter};
use std::ops::{Deref, DerefMut};

#[derive(Debug, Eq)]
pub struct GornTree<V>(BTreeMap<Vec<usize>, V>);

impl<V> GornTree<V> {
    pub fn new() -> GornTree<V> {
        GornTree(BTreeMap::new())
    }
}

impl<V> Deref for GornTree<V> {
    type Target = BTreeMap<Vec<usize>, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V> DerefMut for GornTree<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<V> IntoIterator for GornTree<V> {
    type Item = (Vec<usize>, V);
    type IntoIter = IntoIter<Vec<usize>, V>;

    fn into_iter(self) -> IntoIter<Vec<usize>, V> {
        self.0.into_iter()
    }
}

impl<'a, V> IntoIterator for &'a GornTree<V> {
    type Item = (&'a Vec<usize>, &'a V);
    type IntoIter = Iter<'a, Vec<usize>, V>;

    fn into_iter(self) -> Iter<'a, Vec<usize>, V> {
        (&self.0).into_iter()
    }
}

impl<V: PartialEq> PartialEq for GornTree<V> {
    fn eq(&self, other: &GornTree<V>) -> bool {
        self.0.eq(&other.0)
    }
}
