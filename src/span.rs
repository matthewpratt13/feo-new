#![allow(dead_code)]

use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    input: Arc<String>,
    start: usize,
    end: usize,
}

impl Span {
    /// Constructor method
    pub fn new(input: &str, start: usize, end: usize) -> Self {
        Span {
            input: Arc::new(input.to_string()),
            start,
            end,
        }
    }

    pub fn input(&self) -> Arc<String> {
        Arc::clone(&self.input)
    }

    /// Method to get the substring corresponding to the span
    pub fn substring(&self) -> &str {
        &self.input[self.start..self.end]
    }
}
