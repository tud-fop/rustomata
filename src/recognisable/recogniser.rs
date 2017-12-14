use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::rc::Rc;
use util::agenda::Agenda;
use util::push_down::Pushdown;

/// Iterator for `recognise` that creates new solutions with every step
pub struct Recogniser<'a, A, C, R: Ord, K: Hash, O> {  // TODO rename to ParseForest
    pub agenda: A,
    pub configuration_characteristic: Box<Fn(&C) -> &K>,
    pub filtered_rules: Rc<HashMap<K, BinaryHeap<R>>>,
    pub apply: Box<Fn(&C, &R) -> Vec<C>>,
    pub accepting: Box<Fn(&C) -> bool>,
    pub item_map: Box<Fn(&(C, Pushdown<R>)) -> O + 'a>,
}

impl<'a, A, C, R, K, O> Iterator for Recogniser<'a, A, C, R, K, O>
    where A: Agenda<Item=(C, Pushdown<R>)>,
          C: Clone + Ord,
          R: Clone + Ord,
          K: Eq + Hash,
{
    type Item = O;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((c, run)) = self.agenda.dequeue() {
            if let Some(rs) = self.filtered_rules.get((self.configuration_characteristic)(&c)) {
                for r in rs {
                    for c1 in (self.apply)(&c, r) {
                        let run1 = run.clone().push(r.clone());
                        self.agenda.enqueue((c1, run1));
                    }
                }
            }
            if (self.accepting)(&c) {
                return Some((self.item_map)(&(c, run)));
            }
        }

        None
    }
}
