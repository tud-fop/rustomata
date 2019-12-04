mod configuration;
mod transition;

pub mod automaton;
pub mod from_str;
#[macro_use]
pub mod coarse_to_fine;

use crate::util::push_down::Pushdown;
use search::agenda::weighted::Weighted;
use std::vec::Vec;

pub use self::configuration::Configuration;
pub use self::transition::Transition;

/// Something we can `apply` to a configuration.
pub trait Instruction {
    type Storage;

    fn apply(&self, _: Self::Storage) -> Vec<Self::Storage>;
}

/// items of the transition system
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Item<S, I, T, W>(
    pub Configuration<S, T, W>,
    pub Pushdown<Transition<I, T, W>>,
);
pub type VecItem<S, I, T, W> = (Configuration<S, T, W>, Vec<Transition<I, T, W>>);

impl<S, I: Instruction<Storage = S>, T, W: Clone> Weighted for Item<S, I, T, W> {
    type Weight = W;

    fn get_weight(&self) -> W {
        self.0.weight.clone()
    }
}

/// Something that recognises words and output corresponding parses.
pub trait Recognisable<T, W> {
    // TODO rename to Recogniser
    type Parse;

    fn recognise<'a>(&'a self, word: Vec<T>) -> Box<dyn Iterator<Item = Self::Parse> + 'a>;

    fn recognise_beam_search<'a>(
        &'a self,
        beam: usize,
        word: Vec<T>,
    ) -> Box<dyn Iterator<Item = Self::Parse> + 'a>;
}
