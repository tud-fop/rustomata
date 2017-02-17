extern crate num_traits;

use std::f64;
use std::cmp::Ordering;
use std::str::FromStr;
use std::ops::{Add, Mul};
use self::num_traits::{One, Zero};

#[derive(PartialOrd, Debug, Default, Clone, Copy)]
pub struct LogProb {
    value: f64
}

impl LogProb {
    pub fn new(value: f64) -> Result<Self, String> {
        if 0.0 <= value && value <= 1.0 {
            Ok(LogProb { value: - value.log2() })
        } else {
            Err(format!("Value {} is not a probability (i.e. not in the interval [0,1]).", value))
        }
    }

    pub fn probability(&self) -> f64 {
        (- &self.value).exp2()
    }
}

/// Flipped instance to reflect that a probability of ``0.25`` is "more expensive" than ``0.5``.
impl Ord for LogProb {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(&other) {
            Some(ordering) => ordering,
            None => {
                if self.value.is_nan() {
                    if other.value.is_nan() {
                        Ordering::Equal
                    } else {
                        Ordering::Greater
                    }
                } else {
                    Ordering::Less
                }
            }
        }
    }
}

impl PartialEq for LogProb {
    fn eq(&self, other: &Self) -> bool {
        if self.value.is_nan() {
            if other.value.is_nan() { true } else { false }
        } else if other.value.is_nan() {
            false
        } else {
            self.value == other.value
        }
    }
}

impl Eq for LogProb {}

impl Mul for LogProb {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        LogProb { value: self.value.add(other.value) }
    }
}

impl Add for LogProb {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match LogProb::new(self.probability().add(other.probability())) {
            Ok(p) => p,
            Err(e) => panic!(e)
        }
    }
}

impl Zero for LogProb {
    fn zero() -> LogProb {
        LogProb { value: f64::INFINITY }
    }

    fn is_zero(&self) -> bool {
        self.value.is_infinite()
    }
}

impl One for LogProb {
    fn one() -> LogProb {
        LogProb { value: 0.0 }
    }
}

impl FromStr for LogProb {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse() {
            Ok(p) => LogProb::new(p),
            Err(e) => Err(e.to_string())
        }
    }
}

