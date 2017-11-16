use std::collections::{BinaryHeap, BTreeMap};
use std::vec::Vec;

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

#[derive(Debug, PartialEq, Eq)]
pub struct BoundedPriorityQueue<P, I> {
    data: BTreeMap<P, Vec<I>>, // The values should always be non-empty.
    capacity: usize,
    size: usize,
    last_key: Option<P>,       // largest key w.r.t. Ord
}

impl<P, I> BoundedPriorityQueue<P, I> {
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn is_at_capacity(&self) -> bool {
        self.size == self.capacity
    }
}

impl<P: Ord + Clone, I: Weighted<Weight=P>> Agenda for BoundedPriorityQueue<P, I> {
    type Item = I;

    fn enqueue(&mut self, item: I) -> Option<I> {
        let priority = item.get_weight();
        if self.size < self.capacity {
            self.enqueue_unchecked(priority, item);
            None
        } else if &priority < self.last_key.as_ref().expect("[ERROR] `last_key` should not be `None` when the queue is non-empty.") {
            self.enqueue_unchecked(priority, item);
            self.drop_last()
        } else {
            Some(item)
        }
    }

    fn dequeue(&mut self) -> Option<I> {
        match match self.data.iter_mut().next() {
            Some((k, v)) => {
                let res = v.pop();
                if res.is_some() {
                    self.size = self.size - 1;
                    if self.size == 0 {
                        self.last_key = None;
                    }
                }
                let key = if v.is_empty() { Some(k.clone()) } else { None };
                (key, res)
            },
            None => (None, None),
        } {
            (Some(k), res) => {
                self.data.remove(&k);
                res
            },
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

impl<P: Ord, I> BoundedPriorityQueue<P, I> {
    pub fn new(capacity: usize) -> BoundedPriorityQueue<P, I> {
        assert!(capacity > 0);
        BoundedPriorityQueue { data: BTreeMap::new(), capacity: capacity, size: 0, last_key: None }
    }
}

impl<P: Ord + Clone, I> BoundedPriorityQueue<P, I> {
    pub fn set_capacity(&mut self, capacity: usize) -> Vec<I>{
        self.capacity = capacity;
        let mut res = Vec::new();
        while self.size > self.capacity {  // TODO optimise to remove entire key-value-pairs at a time
            res.push(self.drop_last().expect("[ERROR] `last_key` should not be `None` when the queue is non-empty."));
        }
        res.reverse();
        res
    }

    fn enqueue_unchecked(&mut self, priority: P, item: I) {
        self.data.entry(priority.clone()).or_insert(Vec::new()).push(item);
        self.last_key = match self.last_key {
            Some(ref lk) if priority < *lk => Some(lk.clone()),
            _ => Some(priority),
        };
        self.size = self.size + 1;
    }

    fn drop_last(&mut self) -> Option<I> {
        match self.last_key.clone() {
            Some(key) => {
                let mut vec = self.data.remove(&key).expect("[ERROR] `last_key` should only hold keys that occur in `data`.");
                let item = vec.pop().expect("[ERROR] `data` should not contain empty `Vec`tors.");

                if !vec.is_empty() {
                    self.data.insert(key.clone(), vec);
                } else {
                    self.last_key = self.data.keys().next_back().cloned();
                }
                self.size = self.size - 1;

                Some(item)
            },
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


#[test]
fn test_bounded_priority_queue() {
    impl Weighted for char {
        type Weight = Self;

        fn get_weight(&self) -> Self::Weight {
            self.clone()
        }
    }

    let mut q = BoundedPriorityQueue::new(5);

    assert_eq!(q.size(), 0);
    assert!(q.is_empty());

    assert_eq!(q.enqueue('i'), None);
    assert_eq!(q.peek_next(), Some(&'i'));
    assert_eq!(q.size(), 1);

    assert_eq!(q.enqueue('h'), None);
    assert_eq!(q.peek_next(), Some(&'h'));
    assert_eq!(q.size(), 2);

    assert_eq!(q.enqueue('g'), None);
    assert_eq!(q.peek_next(), Some(&'g'));
    assert_eq!(q.size(), 3);

    assert_eq!(q.enqueue('a'), None);
    assert_eq!(q.peek_next(), Some(&'a'));
    assert_eq!(q.size(), 4);

    assert_eq!(q.enqueue('f'), None);
    assert_eq!(q.peek_next(), Some(&'a'));

    assert_eq!(q.size(), 5);
    assert!(q.is_at_capacity());

    assert_eq!(q.enqueue('e'), Some(('i')));

    assert_eq!(q.set_capacity(7), vec![]);

    assert_eq!(q.enqueue('c'), None);
    assert_eq!(q.enqueue('b'), None);

    assert_eq!(q.set_capacity(5), vec![('g'), ('h')]);

    assert_eq!(q.size(), 5);
    assert!(q.is_at_capacity());

    assert_eq!(q.dequeue(), Some('a'));
    assert_eq!(q.dequeue(), Some('b'));
    assert_eq!(q.dequeue(), Some('c'));
    assert_eq!(q.dequeue(), Some('e'));
    assert_eq!(q.dequeue(), Some('f'));
    assert_eq!(q.dequeue(), None);
}
