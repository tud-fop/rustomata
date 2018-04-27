use std::collections::{BTreeMap, BinaryHeap};
use std::vec::Vec;

/// A limit specification.
#[derive(PartialEq, PartialOrd, Eq, Ord, Copy, Clone, Debug)]
pub enum Capacity {
    Limit(usize),
    Infinite,
}

pub trait Agenda {
    type Item;

    fn enqueue(&mut self, Self::Item) -> Option<Self::Item>;
    fn dequeue(&mut self) -> Option<Self::Item>;
    fn peek_next(&self) -> Option<&Self::Item>;
    fn is_empty(&self) -> bool;
}

pub trait Weighted {
    type Weight;

    fn get_weight(&self) -> Self::Weight;
}

pub struct Queue<I> {
    data: Vec<I>,
    capacity: Capacity
}

impl<I> Agenda for Queue<I> {
    type Item = I;
    
    fn enqueue(&mut self, item: I) -> Option<I> {
        self.data.push(item);
        
        if Capacity::Limit(self.data.len()) <= self.capacity {
            None
        } else {
            Some(self.data.remove(0))
        }
    }
    
    fn dequeue(&mut self) -> Option<I> {
        self.data.pop()
    }
    
    fn peek_next(&self) -> Option<&I> {
        self.data.last()
    }
    
    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl<T> Queue<T> {
    pub fn set_capacity(&mut self, c: usize) {
        self.capacity = Capacity::Limit(c);
    }
    pub fn new() -> Self {
        Queue{ data: Vec::new(), capacity: Capacity::Infinite }
    }
}

impl<T> ::std::iter::FromIterator<T> for Queue<T> {
    fn from_iter<I>(iter: I) -> Self where I: IntoIterator<Item=T> {
        Queue{ capacity: Capacity::Infinite, data: iter.into_iter().collect() }
    }
}

// #[derive(Debug, PartialEq, Eq)]
pub struct PriorityQueue<P, I>
where
    I: Weighted<Weight = P>,
{
    data: BTreeMap<P, Vec<I>>, // The values should always be non-empty.
    capacity: Capacity,
    size: usize,
    last_key: Option<P>, // largest key w.r.t. Ord
                         // pub priority: Box<Fn(&I) -> P + 'a>,
}

impl<P, I> ::std::iter::FromIterator<I> for PriorityQueue<P, I>
where
    I: Weighted<Weight = P>,
    P: Ord + Clone
{
    fn from_iter<It>(iter: It) -> Self
    where
        It: IntoIterator<Item=I>
    {
        let mut q = PriorityQueue::new(Capacity::Infinite);
        for element in iter {
            q.enqueue(element);
        }
        q
    }
}

impl<P, I> PriorityQueue<P, I>
where
    I: Weighted<Weight = P>,
{
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn capacity(&self) -> Capacity {
        self.capacity
    }

    pub fn is_at_capacity(&self) -> bool {
        Capacity::Limit(self.size) == self.capacity
    }
}

impl<I, P: Ord + Clone> Agenda for PriorityQueue<P, I>
where
    I: Weighted<Weight = P>,
{
    type Item = I;

    fn enqueue(&mut self, item: I) -> Option<I> {
        let priority = item.get_weight();

        if Capacity::Limit(self.size) < self.capacity {
            self.enqueue_unchecked(priority, item);
            None
        } else if &priority <
                   self.last_key.as_ref().expect(
                "[ERROR] `last_key` should not be `None` when the queue is non-empty.",
            )
        {
            self.enqueue_unchecked(priority, item);
            self.drop_last()
        } else {
            Some(item)
        }
    }

    fn dequeue(&mut self) -> Option<I> {
        match match self.data.iter_mut().next() {
            Some((k, v)) => {
                let res = if v.len() > 0 {
                    self.size -= 1;
                    if self.size == 0 {
                        self.last_key = None;
                    }
                    Some(v.remove(0))
                } else {
                    None
                };
                let key = if v.is_empty() { Some(k.clone()) } else { None };
                (key, res)
            }
            None => (None, None),
        } {
            (Some(k), res) => {
                self.data.remove(&k);
                res
            }
            (None, res) => res,
        }
    }

    fn peek_next(&self) -> Option<&I> {
        self.data.values().next().and_then(|v| v.last())
    }

    fn is_empty(&self) -> bool {
        self.size == 0
    }
}

impl<P: Ord, I> PriorityQueue<P, I>
where
    I: Weighted<Weight = P>,
{
    pub fn new(capacity: Capacity) -> PriorityQueue<P, I> {
        assert!(capacity > Capacity::Limit(0));
        PriorityQueue {
            data: BTreeMap::new(),
            capacity,
            size: 0,
            last_key: None,
        }
    }
}

impl<P: Ord + Clone, I> PriorityQueue<P, I>
where
    I: Weighted<Weight = P>,
{
    pub fn set_capacity(&mut self, capacity: usize) -> Vec<I> {
        self.capacity = Capacity::Limit(capacity);
        let mut res = Vec::new();
        while Capacity::Limit(self.size) > self.capacity {
            // TODO optimise to remove entire key-value-pairs at a time
            res.push(self.drop_last().expect(
                "[ERROR] `last_key` should not be `None` when the queue is non-empty.",
            ));
        }
        res.reverse();
        res
    }

    fn enqueue_unchecked(&mut self, priority: P, item: I) {
        self.data
            .entry(priority.clone())
            .or_insert_with(Vec::new)
            .push(item);
        self.last_key = match self.last_key {
            Some(ref lk) if priority < *lk => Some(lk.clone()),
            _ => Some(priority),
        };
        self.size += 1;
    }

    fn drop_last(&mut self) -> Option<I> {
        match self.last_key.clone() {
            Some(key) => {
                let mut vec = self.data.remove(&key).expect(
                    "[ERROR] `last_key` should only hold keys that occur in `data`.",
                );
                let item = vec.pop().expect(
                    "[ERROR] `data` should not contain empty `Vec`tors.",
                );

                if !vec.is_empty() {
                    self.data.insert(key.clone(), vec);
                } else {
                    self.last_key = self.data.keys().next_back().cloned();
                }
                self.size -= 1;

                Some(item)
            }
            None => None,
        }
    }
}

impl<I: Ord> Agenda for BinaryHeap<I> {
    type Item = I;

    fn enqueue(&mut self, item: Self::Item) -> Option<Self::Item> {
        self.push(item);
        None
    }

    fn dequeue(&mut self) -> Option<Self::Item> {
        self.pop()
    }

    fn peek_next(&self) -> Option<&Self::Item> {
        self.peek()
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<I> Agenda for Vec<I> {
    type Item = I;

    fn enqueue(&mut self, item: Self::Item) -> Option<Self::Item> {
        self.push(item);
        None
    }

    fn dequeue(&mut self) -> Option<Self::Item> {
        self.pop()
    }

    fn peek_next(&self) -> Option<&Self::Item> {
        self.last()
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn capacity() {
        for (i, j) in vec![(0, 1), (9, 10), (0, 100)] {
            assert!(Capacity::Limit(i) < Capacity::Limit(j));
            assert!(Capacity::Limit(i) < Capacity::Infinite);
            assert!(Capacity::Limit(j) < Capacity::Infinite);
        }
    }

    #[test]
    fn test_bounded_priority_queue() {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
        struct Item(char);
        impl Weighted for Item {
            type Weight = char;
            fn get_weight(&self) -> char {
                match *self {
                    Item(c) => c,
                }
            }
        }

        let mut q = PriorityQueue::new(Capacity::Limit(5));

        assert_eq!(q.size(), 0);
        assert!(q.is_empty());

        assert_eq!(q.enqueue(Item('i')), None);
        assert_eq!(q.peek_next(), Some(&Item('i')));
        assert_eq!(q.size(), 1);

        assert_eq!(q.enqueue(Item('h')), None);
        assert_eq!(q.peek_next(), Some(&Item('h')));
        assert_eq!(q.size(), 2);

        assert_eq!(q.enqueue(Item('g')), None);
        assert_eq!(q.peek_next(), Some(&Item('g')));
        assert_eq!(q.size(), 3);

        assert_eq!(q.enqueue(Item('a')), None);
        assert_eq!(q.peek_next(), Some(&Item('a')));
        assert_eq!(q.size(), 4);

        assert_eq!(q.enqueue(Item('f')), None);
        assert_eq!(q.peek_next(), Some(&Item('a')));

        assert_eq!(q.size(), 5);
        assert!(q.is_at_capacity());

        assert_eq!(q.enqueue(Item('e')), Some(Item('i')));

        assert_eq!(q.set_capacity(7), vec![]);

        assert_eq!(q.enqueue(Item('c')), None);
        assert_eq!(q.enqueue(Item('b')), None);

        assert_eq!(q.set_capacity(5), vec![(Item('g')), (Item('h'))]);

        assert_eq!(q.size(), 5);
        assert!(q.is_at_capacity());

        assert_eq!(q.dequeue(), Some(Item('a')));
        assert_eq!(q.dequeue(), Some(Item('b')));
        assert_eq!(q.dequeue(), Some(Item('c')));
        assert_eq!(q.dequeue(), Some(Item('e')));
        assert_eq!(q.dequeue(), Some(Item('f')));
        assert_eq!(q.dequeue(), None);
    }
}