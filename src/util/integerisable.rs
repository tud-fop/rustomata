use integeriser::Integeriser;

pub trait Integerisable1
where
    Self::I: Integeriser,
{
    type AInt;
    /// type of the integerised self
    type I;
    /// type of the integeriser

    fn integerise(&self, integeriser: &mut Self::I) -> Self::AInt;

    fn un_integerise(_: &Self::AInt, integeriser: &Self::I) -> Self;
}

pub trait Integerisable2
where
    Self::I1: Integeriser,
    Self::I2: Integeriser,
{
    type AInt;
    /// type of the integerised self
    type I1;
    /// type of the first integeriser
    type I2;
    /// type of the second integeriser

    fn integerise(&self, integeriser1: &mut Self::I1, integeriser2: &mut Self::I2) -> Self::AInt;

    fn un_integerise(_: &Self::AInt, integeriser1: &Self::I1, integeriser2: &Self::I2) -> Self;
}
