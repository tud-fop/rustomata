use log_domain::LogDomain;
use std::{ iter::repeat, marker::Sized };


pub trait Factorizable
where
    Self: Sized
{
    fn factorize(self, n: usize) -> Vec<Self>;
}

impl Factorizable for LogDomain<f64> {
    fn factorize(self, n: usize) -> Vec<Self> {
        let factors = self.pow(1.0 / n as f64);
        repeat(factors).take(n).collect()
    }
}

impl Factorizable for () {
    fn factorize(self, n: usize) -> Vec<Self> {
        repeat(()).take(n).collect()
    }
}