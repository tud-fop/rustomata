use std::iter::DoubleEndedIterator;
use std::ops::{Index, IndexMut};

pub struct VecMultiMap<T>(Vec<Vec<T>>);
pub struct VecMultiMapAdapter<'a, T>(pub &'a mut Vec<Vec<T>>);

impl<'a, T> VecMultiMapAdapter<'a, T> {
    fn pad_to(&mut self, index: usize) {
        if index >= self.0.len() { self.0.resize_with(index + 1, Vec::new) }
    }
    
    pub fn push_to(&mut self, index: usize, value: T) {
        self.pad_to(index);
        self.0[index].push(value);
    }

    pub fn get(&mut self, index: usize) -> &Vec<T> {
        self.pad_to(index);
        unsafe { self.0.get_unchecked(index) }
    }
    
    pub fn get_or_fail(&self, index: usize) -> Option<&Vec<T>> {
        self.0.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> &mut Vec<T> {
        self.pad_to(index);
        unsafe { self.0.get_unchecked_mut(index) }
    }
}

impl<T> VecMultiMap<T> {
    pub fn new() -> Self { VecMultiMap(Vec::new()) }

    fn pad_to(&mut self, index: usize) {
        if index >= self.0.len() { self.0.resize_with(index + 1, Vec::new) }
    }

    pub fn push_to(&mut self, index: usize, value: T) {
        self.pad_to(index);
        self.0[index].push(value);
    }

    pub fn get(&mut self, index: usize) -> &Vec<T> {
        self.pad_to(index);
        unsafe { self.0.get_unchecked(index) }
    }
    
    pub fn get_or_fail(&self, index: usize) -> Option<&Vec<T>> {
        self.0.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> &mut Vec<T> {
        self.pad_to(index);
        unsafe { self.0.get_unchecked_mut(index) }
    }

    pub fn into_vec_with_size(mut self, size: usize) -> Vec<Vec<T>> {
        self.0.resize_with(size, Vec::new);
        self.0
    }
}

impl<'a, T> From<&'a mut Vec<Vec<T>>> for VecMultiMapAdapter<'a, T> {
    fn from(v: &'a mut Vec<Vec<T>>) -> Self { Self(v) }
}

impl<T> Into<Vec<Vec<T>>> for VecMultiMap<T> {
    fn into(self) -> Vec<Vec<T>> {
        // find greatest index with non-empty vector
        if let Some((last_filled_index, _)) = self.0.iter().enumerate().rfind(|(_, v)| v.len() > 0) {
            self.into_vec_with_size(last_filled_index + 1)
        } else {
            Vec::new()
        }
    }
}


impl<'a, T> Index<usize> for VecMultiMapAdapter<'a, T> {
    type Output = Vec<T>;
    fn index(&self, index: usize) -> &Self::Output {
        self.get_or_fail(index).expect("immutable index out of bounds")
    }
}
impl<'a, T> IndexMut<usize> for VecMultiMapAdapter<'a, T> {
    fn index_mut(&mut self, index: usize) -> &mut <Self as Index<usize>>::Output {
        self.get_mut(index)
    }
}