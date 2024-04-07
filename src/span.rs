#![allow(dead_code)]

use std::sync::Arc;

/// Struct that represents a range of characters in the source code where a token appears.
/// This information is useful in numerous cases, including displaying error messages
/// with exact locations, indicating syntax errors and transforming code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    input: Arc<String>,
    start: usize,
    end: usize,
}

impl Span {
    /// Constructor method.
    /// Create a new `Span` with a `start` and `end` position in some input string.
    /// Use `Arc` to share references to this source code among different threads.
    pub fn new(input: &str, start: usize, end: usize) -> Self {
        Span {
            input: Arc::new(input.to_string()),
            start,
            end,
        }
    }

    /// Retrieve a reference to the  source code.
    pub fn input(&self) -> Arc<String> {
        Arc::clone(&self.input)
    }

    /// Method to get the substring corresponding to the span.
    pub fn substring(&self) -> &str {
        &self.input[self.start..self.end]
    }
}
