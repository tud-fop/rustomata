
//used to map the weight changes
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TransitionKey<I, T,>{
    pub instruction : I,
    pub word: Vec<T>,
}

//function to add up identical transitions
pub trait Redundancy{
    fn reduce_redundancy(self) -> Self;
}
