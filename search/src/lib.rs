#![feature(test)]

extern crate test;

pub mod agenda;
pub mod search;

pub use crate::agenda::{Agenda, BeamHeap, LimitedHeap};
pub use crate::search::Search;

#[cfg(test)]
mod tests {
    use rand::{rngs::SmallRng, Rng, SeedableRng};

    use super::test::{black_box, Bencher};
    use super::BeamHeap;
    use super::LimitedHeap;
    use min_max_heap::MinMaxHeap;
    use std::collections::BinaryHeap;

    const CAP: usize = 500;
    const ELM: usize = 1000;
    const SEED: [u8; 16] = [0; 16];

    #[bench]
    fn minmax_insert(b: &mut Bencher) {
        let mut rng: SmallRng = <SmallRng as SeedableRng>::from_seed(SEED);
        let mut q = LimitedHeap::with_capacity(CAP);
        let elements = (0..ELM).map(|_| rng.gen_range(0, ELM)).collect::<Vec<_>>();

        b.iter(|| {
            q.clear();
            for element in elements.iter() {
                black_box(q.push(*element, *element));
            }
        });
    }

    #[bench]
    fn minmaxheap_insert(b: &mut Bencher) {
        let mut rng: SmallRng = <SmallRng as SeedableRng>::from_seed(SEED);
        let mut q = MinMaxHeap::with_capacity(CAP);
        let elements = (0..ELM).map(|_| rng.gen_range(0, ELM)).collect::<Vec<_>>();

        b.iter(|| {
            q.clear();
            for element in elements.iter() {
                black_box(q.push(*element));
            }
        });
    }

    #[bench]
    fn heap_insert(b: &mut Bencher) {
        let mut rng: SmallRng = <SmallRng as SeedableRng>::from_seed(SEED);
        let mut q = BinaryHeap::with_capacity(CAP);
        let elements = (0..ELM).map(|_| rng.gen_range(0, ELM)).collect::<Vec<_>>();

        b.iter(|| {
            q.clear();
            for element in elements.iter() {
                black_box(q.push(*element));
            }
        });
    }

    #[bench]
    fn beamheap_insert(b: &mut Bencher) {
        let mut rng: SmallRng = <SmallRng as SeedableRng>::from_seed(SEED);
        let mut q = BeamHeap::new(0);
        let elements = (0..ELM).map(|_| rng.gen_range(0, ELM)).collect::<Vec<_>>();

        b.iter(|| {
            q.clear();
            for element in elements.iter() {
                black_box(q.push(*element, *element));
            }
        });
    }

    // # pop

    #[bench]
    fn minmax_pop(b: &mut Bencher) {
        let mut rng: SmallRng = <SmallRng as SeedableRng>::from_seed(SEED);
        let mut q = LimitedHeap::with_capacity(CAP);
        for _ in 0..ELM {
            let element = rng.gen_range(0, ELM);
            black_box(q.push(element, element));
        }

        b.iter(|| {
            let mut inner_q = q.clone();
            for _ in 0..CAP {
                black_box(inner_q.pop());
            }
        });
    }

    #[bench]
    fn heap_pop(b: &mut Bencher) {
        let mut rng: SmallRng = <SmallRng as SeedableRng>::from_seed(SEED);
        let mut q = BinaryHeap::with_capacity(CAP);
        for _ in 0..CAP {
            black_box(q.push(rng.gen_range(0, ELM)));
        }

        b.iter(|| {
            let mut inner_q = q.clone();
            for _ in 0..CAP {
                black_box(inner_q.pop());
            }
        });
    }

    #[bench]
    fn minmaxheap_pop(b: &mut Bencher) {
        let mut rng: SmallRng = SeedableRng::from_seed(SEED);
        let mut q = MinMaxHeap::with_capacity(CAP);
        for _ in 0..CAP {
            black_box(q.push(rng.gen_range(0, ELM)));
        }

        b.iter(|| {
            let mut inner_q = q.clone();
            for _ in 0..CAP {
                black_box(inner_q.pop_max());
            }
        });
    }

    #[bench]
    fn beamheap_pop(b: &mut Bencher) {
        let mut rng: SmallRng = SeedableRng::from_seed(SEED);
        let mut q = BeamHeap::new(0);
        for _ in 0..CAP {
            let random = rng.gen_range(0, ELM);
            black_box(q.push(random, random));
        }

        b.iter(|| {
            let mut inner_q = q.clone();
            for _ in 0..CAP {
                black_box(inner_q.pop());
            }
        });
    }
}
