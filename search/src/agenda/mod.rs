//! This module provides data structures that we use to hold elements during a
//! search and a unified interface to them in the `Agenda` trait.
//! 
//! * In general, we order elements max-first in this module. So, like
//!   `std::collections::heap`, we will return the element with the greatest
//!   priority when we pop from a weighted data structure.
//! 
//! * In this module, we define `Agenda`s with prioritized elements. In all
//!   interfaces, we separated the elements and priorities. So, the elements
//!   themselves do not need to implement `Ord`.


pub mod weighted;
pub mod beam_heap;
pub mod limited_heap;
pub mod binary_heap;

pub use self::limited_heap::LimitedHeap;
pub use self::beam_heap::BeamHeap;
use self::weighted::Weighted;

use std::{ ops::Mul, collections::VecDeque };

/// Generic interface to a data structure that can hold some amount of elements
/// of type `Agenda::Item`.
pub trait Agenda {
    type Item;
    fn push(&mut self, element: Self::Item);
    fn pop(&mut self) -> Option<Self::Item>;
    fn peek(&self) -> Option<&Self::Item>;
    fn len(&self) -> usize;


    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    
    fn extend<I: IntoIterator<Item=Self::Item>>(&mut self, elements: I) {
        for element in elements {
            self.push(element);
        }
    }
}

impl<I> Agenda for Vec<I> {
    type Item = I;
    
    fn push(&mut self, element: Self::Item) {
        self.push(element);
    }
    
    fn pop(&mut self) -> Option<Self::Item> {
        self.pop()
    }
    
    fn peek(&self) -> Option<&Self::Item> {
        self.last()
    }
    
    fn len(&self) -> usize {
        self.len()
    }
}

impl<I> Agenda for VecDeque<I> {
    type Item = I;
    
    fn push(&mut self, element: Self::Item) {
        self.push_front(element);
    }
    
    fn pop(&mut self) -> Option<Self::Item> {
        self.pop_back()
    }
    
    fn peek(&self) -> Option<&Self::Item> {
        self.back()
    }
    
    fn len(&self) -> usize {
        self.len()
    }
}

impl<I: Weighted> Agenda for binary_heap::weighted::BinaryHeap<I>
where
    I::Weight: Ord
{
    type Item = I;
    
    fn push(&mut self, element: Self::Item) {
        self.push(element);
    }
    
    fn pop(&mut self) -> Option<Self::Item> {
        self.pop()
    }
    
    fn peek(&self) -> Option<&Self::Item> {
        self.peek()
    }
    
    fn len(&self) -> usize {
        self.len()
    }
}

impl<I: Weighted> Agenda for beam_heap::weighted::BeamHeap<I>
where
    I::Weight: Ord + Mul<Output=I::Weight> + Clone
{
    type Item = I;
    
    fn push(&mut self, element: Self::Item) {
        self.push(element);
    }
    
    fn pop(&mut self) -> Option<Self::Item> {
        self.pop()
    }
    
    fn peek(&self) -> Option<&Self::Item> {
        self.peek()
    }
    
    fn len(&self) -> usize {
        self.len()
    }
}

impl<I: Weighted> Agenda for limited_heap::weighted::LimitedHeap<I>
where
    I::Weight: Ord
{
    type Item = I;
    
    fn push(&mut self, element: Self::Item) {
        self.push(element);
    }
    
    fn pop(&mut self) -> Option<Self::Item> {
        self.pop()
    }
    
    fn peek(&self) -> Option<&Self::Item> {
        self.peek()
    }
    
    fn len(&self) -> usize {
        self.len()
    }
}