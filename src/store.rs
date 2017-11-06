// GCed store for the ATL runtime
use std::ops::Index;

#[derive(Debug, Clone, PartialEq)]
pub struct Store {
    store: Vec<usize>,
}

// Store Tuples and Closures
// Closures are (N + 1, body-pc, capture1 ... captureN)
// Tuples are (N, item1 ... itemN)
impl Store {

    pub fn new() -> Store {
        Store { store: vec![] }
    }

    pub fn add(&mut self, value: usize) -> usize {
        self.store.push(value);
        self.store.len() - 1
    }

    pub fn new_tuple(&mut self, data: &[usize]) -> usize {
        let base_pointer = self.store.len();
        self.store.reserve(data.len());
        self.store.extend_from_slice(data);
        base_pointer
    }

    pub fn new_closure(&mut self, pc: usize, data: &[usize]) -> usize {
        let base_pointer = self.store.len();
        self.store.reserve(data.len() + 2);
        self.store.push(data.len() + 1);
        self.store.push(pc);
        self.store.extend_from_slice(data);
        base_pointer
    }

    pub fn test_view(&self) -> &Vec<usize> {
        &self.store
    }
}

impl Index<usize> for Store {
    type Output = usize;
    fn index(&self, pointer: usize) -> &usize {
        &self.store[pointer]
    }
}
