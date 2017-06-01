extern crate num_traits;

use std::f64;
use std::cmp::Ordering;
use std::fmt;
use std::str::FromStr;
use std::ops::{Add, Sub, Mul, Div};
use self::num_traits::{One, Zero};

#[derive(PartialOrd, Debug, Default, Clone, Copy)]
pub struct LogProb {
    value: f64
}

impl LogProb {
    pub fn new(value: f64) -> Result<Self, String> {
        if 0.0 <= value && value <= 1.0 {
            Ok(LogProb { value: - value.ln() })
        } else {
            Err(format!("{} is not a probability (i.e. not in the interval [0,1]).", value))
        }
    }

    pub fn probability(&self) -> f64 {
        (- &self.value).exp()
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
            self.value - other.value < f64::EPSILON
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

        match x - (x - y).exp().ln_1p() {
            s if s >= 0.0 => LogProb { value: s },
            _             => panic!(format!("exp(-{}) + exp(-{}) is not a probability, i.e. not in the interval [0,1].", x, y)),
        }
    }
}

impl Sub for LogProb {
    type Output = Self ;

    fn sub(self, other: Self) -> Self {
        match (self.value, other.value) {
            (x, y) if x <= y => LogProb { value: (x - (-(x - y).exp_m1()).ln()) },
            (x, y) if x >  y => panic!(format!("exp(-{}) - exp(-{}) is not a probability, i.e. not on the interval [0,1].", x, y)),
            _                => unreachable!(),
        }
    }
}

#[test]
fn test_add_sub() {
    match (LogProb::new(0.5), LogProb::new(0.25), LogProb::new(0.75)) {
        (Ok(x), Ok(y), Ok(z)) => {
            assert_eq!(x + y, z);
            assert_eq!(y + x, z);
            assert_eq!(z - x, y);
            assert_eq!(z - y, x);
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
        match self.value.sub(other.value) {
            x if x >= 0.0
                => LogProb { value: x },
            x if x <  0.0
                => panic!(format!("{} is not a probability (i.e. not in the interval [0,1]).", x)),
            _   => unreachable!(),
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

impl fmt::Display for LogProb {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.probability())
    }
}
