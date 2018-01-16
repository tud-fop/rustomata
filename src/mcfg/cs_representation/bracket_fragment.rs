use std::fmt::{Display, Error, Formatter};
use super::automata::Delta;

/// The set of all `BracketFragemnt`s Δ' is a finite 
/// subset of all bracket words Δ' ⊂ Δ* with respect
/// to some `MCFG`.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Serialize, Deserialize)]
pub struct BracketFragment<T: PartialEq>(pub Vec<Delta<T>>);

impl<T> BracketFragment<T>
where
    T: PartialEq,
{
    /// Concatenates a sequence of `BracketFragments` ∈ Δ'*.
    /// Since each `BracketFragemnt` is a bracket word
    /// Δ' ⊂ Δ*, `concat(δ) ∈ Δ*` is a bracket word as well.
    pub fn concat(fragments: Vec<Self>) -> Vec<Delta<T>> {
        fragments.into_iter().fold(Vec::new(), |mut bs, f| {
            bs.extend(f.0.into_iter());
            bs
        })
    }
}

impl<T> Display for BracketFragment<T>
where
    T: Display + PartialEq,
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let BracketFragment(ref brackets) = *self;
        let strings: Vec<String> = brackets.iter().map(|b| format!("{}", b)).collect();
        write!(f, "{}", strings.join(""))
    }
}
