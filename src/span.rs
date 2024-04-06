#![allow(dead_code)]



/// Struct that represents a range of characters in the source code where a token appears.
/// This information is useful in numerous cases, including displaying error messages
/// with exact locations, indicating syntax errors and transforming code.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span<'a> {
    input: &'a str,
    start: usize,
    end: usize,
}

impl<'a> Span<'a> {
    /// Constructor method.
    /// Create a new `Span` with a `start` and `end` position in some input string.
    /// Use `Arc` to share references to this source code among different threads.
    pub fn new(input: &'a str, start: usize, end: usize) -> Self {
        Span { input, start, end }
    }

    /// Retrieve a reference to the  source code.
    pub fn input(&self) -> &'a str {
        &self.input
    }

    /// Method to get the substring corresponding to the span.
    pub fn substring(&self) -> &str {
        &self.input[self.start..self.end]
    }
}
