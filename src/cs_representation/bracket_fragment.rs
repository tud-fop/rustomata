use std::fmt::{Display, Error, Formatter};
use cs_representation::BracketContent;
use dyck::Bracket;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Serialize, Deserialize)]
pub struct BracketFragment<T: PartialEq>(pub Vec<Bracket<BracketContent<T>>>);

impl<T> BracketFragment<T>
where
    T: PartialEq
{
    pub fn concat(fragments: Vec<&Self>) -> Vec<&Bracket<BracketContent<T>>> {
        fragments.into_iter()
                 .fold(
                     Vec::new(),
                     | mut bs, f | {
                        bs.extend(f.0.iter());
                        bs
                     }
                 )
    }

    pub fn concat_owned(fragments: Vec<Self>) -> Vec<Bracket<BracketContent<T>>> {
        fragments.into_iter()
                 .fold(
                     Vec::new(),
                     | mut bs, f | {
                        bs.extend(f.0.into_iter());
                        bs
                     }
                 )
    }
}

impl<T> Display for BracketFragment<T>
where
    T: Display + PartialEq
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let BracketFragment(ref brackets) = *self;
        let strings: Vec<String> = brackets.iter().map(|b| format!("{}", b)).collect();
        write!(f, "{}", strings.join(""))
    }

}