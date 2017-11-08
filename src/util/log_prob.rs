extern crate num_traits;

use std::f64;
use std::cmp::Ordering;
use std::fmt;
use std::str::FromStr;
use std::ops::{Add, Sub, Mul, Div};
use self::num_traits::{One, Zero};


/// A struct that represents probabilities.
/// A probability is internally represented as its negative natural logarithm.
/// Probabilities greater than 1 are allowed during calculations.
#[derive(PartialOrd, Debug, Clone, Copy)]
pub enum LogProb { Exp(f64) }

impl LogProb {
    /// Creates a new `LogProb` from a given value in the interval [0,∞).
    pub fn new(value: f64) -> Result<Self, String> {
        if 0.0 <= value {
            Ok(LogProb::new_unchecked(value.ln()))
        } else {
            Err(format!("{} is not a probability, i.e. not in the interval [0,∞).", value))
        }
    }

    fn ln(&self) -> f64 {
        match self {
            &LogProb::Exp(value) => value,
        }
    }

    /// Same as `new` but without bounds check.
    #[inline]
    pub fn new_unchecked(value: f64) -> Self {
        LogProb::Exp(value.ln())
    }

    /// Probability that is represented by the given `LogProb`.
    pub fn probability(&self) -> f64 {
        self.ln().exp()
    }
}

impl Ord for LogProb {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(&other) {
            Some(ordering) => ordering,
            None => {
                if self.ln().is_nan() {
                    if other.ln().is_nan() {
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
        if self.ln().is_nan() {
            if other.ln().is_nan() { true } else { false }
        } else if other.ln().is_nan() {
            false
        } else {
            (self.ln() - other.ln()).abs() <= f64::EPSILON
        }
    }
}

impl Eq for LogProb {}

impl Add for LogProb {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let (a, b) = (self.ln(), other.ln());

        let (x, y) = if a > b {
            (a, b)
        } else {
            (b, a)
        };

        LogProb::new_unchecked(x + (y - x).exp().ln_1p())
    }
}

impl Sub for LogProb {
    type Output = Self ;

    fn sub(self, other: Self) -> Self {
        match (self.ln(), other.ln()) {
            (x, y) if x >= y => LogProb::new_unchecked(x + (-(y - x).exp_m1()).ln()),
            (x, y) if x <  y => panic!("exp({}) - exp({}) is less than zero", x, y),
            _                => unreachable!(),
        }
    }
}

#[test]
fn test_arithmetic() {
    match (LogProb::new(0.5), LogProb::new(0.25), LogProb::new(0.75)) {
        (Ok(x), Ok(y), Ok(z)) => {
            assert_eq!(x + y, z); // 0.5  + 0.25 = 0.75
            assert_eq!(y + x, z); // 0.25 + 0.5  = 0.75
            assert_eq!(z - x, y); // 0.75 - 0.5  = 0.25
            assert_eq!(z - y, x); // 0.75 - 0.25 = 0.5
            assert_eq!(x * x, y); // 0.5  ⋅ 0.5  = 0.25
            assert_eq!(y / x, x); // 0.25 / 0.5  = 0.5
            assert_eq!(z / z, LogProb::one());
                                  // 0.75 / 0.75 = 1
            assert!(z > y);       // 0.75 > 0.25
        },
        _ => panic!(),
    }
}

impl Mul for LogProb {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        LogProb::new_unchecked(self.ln().add(other.ln()))
    }
}

impl Div for LogProb {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        LogProb::new_unchecked(self.ln().sub(other.ln()))
    }
}

impl Zero for LogProb {
    fn zero() -> LogProb {
        LogProb::new_unchecked(f64::NEG_INFINITY)
    }

    fn is_zero(&self) -> bool {
        self.ln() == f64::NEG_INFINITY
    }
}

impl One for LogProb {
    fn one() -> LogProb {
        LogProb::new_unchecked(0.0)
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

impl fmt::Display for LogProb {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.probability())
    }
}
