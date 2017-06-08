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
#[derive(PartialOrd, Debug, Default, Clone, Copy)]
pub struct LogProb {
    value: f64
}

impl LogProb {
    /// Creates a new `LogProb` from a given value in the interval [0,1].
    pub fn new(value: f64) -> Result<Self, String> {
        if 0.0 <= value {
            Ok(LogProb { value: value.ln() })
        } else {
            Err(format!("{} is not a probability, i.e. not in the interval [0,∞).", value))
        }
    }

    /// Same as `new` but without bounds check.
    pub fn new_unchecked(value: f64) -> Self {
        LogProb { value: value.ln() }
    }

    /// Probability that is represented by the given `LogProb`.
    pub fn probability(&self) -> f64 {
        self.value.exp()
    }
}

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
            (self.value - other.value).abs() <= f64::EPSILON
        }
    }
}

impl Eq for LogProb {}

impl Add for LogProb {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let (a, b) = (self.value, other.value);

        let (x, y) = if a > b {
            (a, b)
        } else {
            (b, a)
        };

        LogProb { value: x + (y - x).exp().ln_1p() }
    }
}

impl Sub for LogProb {
    type Output = Self ;

    fn sub(self, other: Self) -> Self {
        match (self.value, other.value) {
            (x, y) if x >= y => LogProb { value: x + (-(y - x).exp_m1()).ln() },
            (x, y) if x <  y => panic!("exp({}) - exp({}) is less than zero", x, y),
            _                => unreachable!(),
        }
    }
}

#[test]
fn test_arithmetic() {
    match (LogProb::new(0.5), LogProb::new(0.25), LogProb::new(0.75)) {
        (Ok(x), Ok(y), Ok(z)) => {
            assert_eq!(x + y, z);
            assert_eq!(y + x, z);
            assert_eq!(z - x, y);
            assert_eq!(z - y, x);
            assert_eq!(x * x, y);
            assert_eq!(y / x, x);
            assert_eq!(z / z, LogProb::one());
            assert!(z > y);
        },
        _ => panic!(),
    }
}

impl Mul for LogProb {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        LogProb { value: self.value.add(other.value) }
    }
}

impl Div for LogProb {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        LogProb { value: self.value.sub(other.value) }
    }
}

impl Add<f64> for LogProb {
    type Output = f64;

    fn add(self, other: f64) -> f64 {
        self.probability().add(other)
    }
}

impl  Div<f64> for LogProb {
    type Output = Self;

    fn div(self, other: f64) -> Self{
        println!("div fault");
        let o;
        match LogProb::new(other) {
            Ok(p) => o=p,
            Err(e) => panic!(e)
        }
        println!("not div fault");
        self/o

    }
}

impl Zero for LogProb {
    fn zero() -> LogProb {
        LogProb { value: f64::NEG_INFINITY }
    }

    fn is_zero(&self) -> bool {
        self.value == f64::NEG_INFINITY
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

impl fmt::Display for LogProb {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.probability())
    }
}
