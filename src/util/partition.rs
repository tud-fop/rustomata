use std::collections::{BTreeSet, BTreeMap};
use std::collections::btree_map::Keys;
use std::rc::Rc;
use std::ops::Deref;

use serde::ser::{Serialize, Serializer};
use serde::de::{Deserialize, Deserializer};

/// A partition Π = {π₁, …, πₙ} of an alpbabet Σ, such that
/// πᵢ ⊆ Σ for each i ∈ [n] and π₁ ∪ … ∪ πₙ = Σ.
#[derive(Debug, PartialEq)]
pub struct Partition<T: Ord>(BTreeMap<T, Rc<BTreeSet<T>>>);

impl<T> Partition<T>
where
    T: Ord + Clone,
{
    /// Takes a list of cells π₁…πₙ and returns a partition if they are pairwise distinct.
    pub fn new(cells: Vec<BTreeSet<T>>) -> Option<Self> {
        let mut partition = BTreeMap::new();

        for cell in cells {
            let ptr = Rc::new(cell);
            for symbol in ptr.iter().cloned() {
                if !partition.insert(symbol, Rc::clone(&ptr)).is_none() {
                    return None;
                }
            }
        }

        Some(Partition(partition))
    }
}

impl<T: Ord> Partition<T> {
    /// Retuns a reference to the cell that contains the symbol `elem`.
    pub fn get_cell(&self, elem: &T) -> Option<&BTreeSet<T>> {
        let &Partition(ref map) = self;

        match map.get(elem) {
            Some(ptr) => Some(ptr.deref()),
            None => None,
        }
    }

    /// Returns a pointer to each cell of the `Partition`.
    pub fn collapse(&self) -> Vec<&BTreeSet<T>> {
        let &Partition(ref map) = self;
        let mut cells: Vec<&BTreeSet<T>> = Vec::new();
        let mut alphabet: BTreeSet<&T> = BTreeSet::new();

        for (sigma, ptr) in map.iter() {
            if !alphabet.contains(sigma) {
                cells.push(ptr.deref());
                alphabet.extend(ptr.deref().iter());
            }
        }

        cells
    }

    /// Returns an iterator over all elements in Σ.
    pub fn alphabet(&self) -> Keys<T, Rc<BTreeSet<T>>> {
        let &Partition(ref map) = self;

        map.keys()
    }
}

impl<T: Serialize + Ord + Clone> Serialize for Partition<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.collapse().serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de> + Ord + Clone> Deserialize<'de> for Partition<T> {
    fn deserialize<D: Deserializer<'de>>(des: D) -> Result<Self, D::Error> {
        match Vec::deserialize(des) {
            Ok(cells) => Ok(Partition::new(cells).unwrap()),
            Err(e) => Err(e),
        }
    }
}


#[cfg(test)]
mod tests {

    #[test]
    fn partition() {
        use std::collections::BTreeSet;

        let cell1: BTreeSet<usize> = vec![1, 2, 3].into_iter().collect();
        let cell2: BTreeSet<usize> = vec![4, 5, 6].into_iter().collect();
        let not_cell = vec![3, 4, 5].into_iter().collect();

        let cells: Vec<BTreeSet<usize>> = vec![cell1.clone(), cell2.clone()];
        let cellref: Vec<&BTreeSet<usize>> = vec![&cell1, &cell2];
        let not_cells = vec![cell1.clone(), not_cell];

        assert!(match super::Partition::new(cells.clone()) {
            None => false,
            Some(partition) => partition.collapse() == cellref,
        });
        assert_eq!(super::Partition::new(not_cells), None);
    }

}
