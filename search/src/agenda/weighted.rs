use std::cmp::Ordering;


/// A trait that gives an interface for a weight assignment.
pub trait Weighted {
    type Weight;

    fn get_weight(&self) -> Self::Weight;
}

impl Weighted for usize {
    type Weight = usize;
    fn get_weight(&self) -> Self::Weight {
        *self
    }
}


/// A `WeightedItem` implements the `PartialEq`, `PartialOrd`, `Eq` and `Ord`
/// traits only with respect to the weight type `W`.
#[derive(Clone)]
pub struct WeightedItem<I, W>(pub I, pub W);

impl<I, W: PartialEq> PartialEq for WeightedItem<I, W> {
    fn eq(&self, other: &Self) -> bool {
        self.1.eq(&other.1)
    }
}

impl<I, W: Eq> Eq for WeightedItem<I, W> {}

impl<I, W: PartialOrd> PartialOrd for WeightedItem<I, W> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.1.partial_cmp(&other.1)
    }
}

impl<I, W: Ord> Ord for WeightedItem<I, W> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.cmp(&other.1)
    }
}

impl<I, W> Weighted for WeightedItem<I, W>
where
    W: Clone
{
    type Weight = W;
    fn get_weight(&self) -> Self::Weight {
        self.1.clone()
    }
}

/// An adapter for `Iterator` that removes the weight type `W` from
/// `WeightedItem` and just yields the item type `I`.
pub struct RemoveWeight<I, W, II>(II) where II: Iterator<Item=WeightedItem<I, W>>;

impl<I, W, Iter, IntoIter> From<IntoIter> for RemoveWeight<I, W, Iter>
where
    IntoIter: IntoIterator<Item=WeightedItem<I, W>,IntoIter=Iter>,
    Iter: Iterator<Item=WeightedItem<I, W>>
{
    fn from(ii: IntoIter) -> Self {
        RemoveWeight(ii.into_iter())
    }
}

impl<I, W, II> Iterator for RemoveWeight<I, W, II>
where
    II: Iterator<Item=WeightedItem<I, W>>
{
    type Item = I;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|wi| wi.0)
    }
}