use std::fmt;

pub struct TryDebug<'a, T: 'a>(pub &'a T);

impl<'a, T: 'a> fmt::Debug for TryDebug<'a, T> {
    default fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "no std::fmt::Debug impl")
    }
}

impl<'a, T: 'a + fmt::Debug> fmt::Debug for TryDebug<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}
