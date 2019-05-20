use super::factorizable::Factorizable;
use num_traits::{One, Zero};
use std::cmp::Ordering;
use std::fmt::{Display, Error, Formatter};
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub, SubAssign};
use std::str::FromStr;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Reverse<W>(W);

impl<W> PartialOrd for Reverse<W>
where
    W: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

impl<W> Ord for Reverse<W>
where
    W: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        other.0.cmp(&self.0)
    }
}

impl<W> Display for Reverse<W>
where
    W: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.0.fmt(f)
    }
}

impl<W> FromStr for Reverse<W>
where
    W: FromStr,
{
    type Err = W::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        W::from_str(s).map(Reverse)
    }
}

impl<W> Zero for Reverse<W>
where
    W: Zero,
{
    fn zero() -> Self {
        Reverse(W::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl<W> One for Reverse<W>
where
    W: One,
{
    fn one() -> Self {
        Reverse(W::one())
    }
}

impl<W> Add for Reverse<W>
where
    W: Add,
{
    type Output = Reverse<W::Output>;
    fn add(self, other: Self) -> Self::Output {
        Reverse(self.0.add(other.0))
    }
}

impl<W> AddAssign for Reverse<W>
where
    W: AddAssign,
{
    fn add_assign(&mut self, other: Self) {
        self.0.add_assign(other.0)
    }
}

impl<W> Sub for Reverse<W>
where
    W: Sub,
{
    type Output = Reverse<W::Output>;
    fn sub(self, other: Self) -> Self::Output {
        Reverse(self.0.sub(other.0))
    }
}

impl<W> SubAssign for Reverse<W>
where
    W: SubAssign,
{
    fn sub_assign(&mut self, other: Self) {
        self.0.sub_assign(other.0)
    }
}

impl<W> Mul for Reverse<W>
where
    W: Mul,
{
    type Output = Reverse<W::Output>;
    fn mul(self, other: Self) -> Self::Output {
        Reverse(self.0.mul(other.0))
    }
}

impl<W> MulAssign for Reverse<W>
where
    W: MulAssign,
{
    fn mul_assign(&mut self, other: Self) {
        self.0.mul_assign(other.0)
    }
}

impl<W> Div for Reverse<W>
where
    W: Div,
{
    type Output = Reverse<W::Output>;
    fn div(self, other: Self) -> Self::Output {
        Reverse(self.0.div(other.0))
    }
}

impl<W> DivAssign for Reverse<W>
where
    W: DivAssign,
{
    fn div_assign(&mut self, other: Self) {
        self.0.div_assign(other.0)
    }
}

impl<W> Rem for Reverse<W>
where
    W: Rem,
{
    type Output = Reverse<W::Output>;
    fn rem(self, other: Self) -> Self::Output {
        Reverse(self.0.rem(other.0))
    }
}

impl<W> RemAssign for Reverse<W>
where
    W: RemAssign,
{
    fn rem_assign(&mut self, other: Self) {
        self.0.rem_assign(other.0)
    }
}

impl<W: Factorizable> Factorizable for Reverse<W> {
    fn factorize(self, n: usize) -> Vec<Self> {
        self.0
            .factorize(n)
            .into_iter()
            .map(Reverse)
            .collect()
    }
}

impl<W> From<W> for Reverse<W> {
    fn from(v: W) -> Self {
        Reverse(v)
    }
}

impl<W> Reverse<W> {
    pub fn unwrap(self) -> W {
        self.0
    }

    pub fn inner(&self) -> &W {
        &self.0
    }
}
