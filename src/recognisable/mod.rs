mod configuration;
mod recogniser;
mod transition;

pub mod automaton;
pub mod from_str;
pub mod int_automaton;

use std::vec::Vec;
use util::agenda::Weighted;
use util::push_down::Pushdown;

pub use self::configuration::Configuration;
pub use self::recogniser::Recogniser;
pub use self::transition::Transition;


/// Something we can `apply` to a configuration.
pub trait Instruction {
    type Storage;

    fn apply(&self, Self::Storage) -> Vec<Self::Storage>;
}


/// items of the transition system
pub type Item<S, I, T, W> = (Configuration<S, T, W>, Pushdown<Transition<I, T, W>>);
pub type VecItem<S, I, T, W> = (Configuration<S, T, W>, Vec<Transition<I, T, W>>);

impl<S, I: Instruction<Storage=S>, T, W: Clone> Weighted for Item<S, I, T, W> {
    type Weight = W;

    fn get_weight(&self) -> W {
        self.0.weight.clone()
    }
}



/// Something that recognises words and output corresponding parses.
pub trait Recognisable<T, W> { // TODO rename to Recogniser
    type Parse;

    fn recognise<'a>(&'a self, word: Vec<T>) -> Box<Iterator<Item=Self::Parse> + 'a>;

    fn recognise_beam_search<'a>(&'a self, beam: usize, word: Vec<T>) -> Box<Iterator<Item=Self::Parse> + 'a>;
}


