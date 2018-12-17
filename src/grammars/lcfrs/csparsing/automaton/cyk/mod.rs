use super::*;

pub fn chart_index(i: u8, j: u8, n: u8) -> usize {
    ( n * (n+1) - (n - (j-i) + 1) * (n - (j-i) + 2) ) / 2 + i
}