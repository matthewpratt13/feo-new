use std::sync::Arc;

pub trait Spanned {
    fn span(&self) -> Span;
}

/// Struct that represents a range of characters in the source code where a token appears.
/// This information is useful in numerous cases, including displaying error messages
/// with exact locations and highlighting syntax errors.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    input: Arc<String>,
    start: usize,
    end: usize,
}

impl Span {
    /// Constructor method.
    /// Create a new `Span` instance with a `start` and `end` position in some input string.
    /// Use `Arc` to share references to this source code among different threads.
    pub(crate) fn new(input: &str, start: usize, end: usize) -> Self {
        Span {
            input: Arc::new(input.to_string()),
            start,
            end,
        }
    }

    /// Get the start position of a given span.
    pub fn start(&self) -> usize {
        self.start
    }

    /// Get the end position of a given span.
    pub fn end(&self) -> usize {
        self.end
    }

    /// Retrieve a thread-safe reference to the source code.
    pub fn input(&self) -> Arc<String> {
        Arc::clone(&self.input)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub col: usize,
    _snippet: String,
}

impl Position {
    pub fn new(pos: usize, source: &str) -> Self {
        let slice = &source[..pos];
        let lines = slice.split('\n').collect::<Vec<_>>();
        let line_count = lines.len();
        let last_line_len = lines.last().unwrap_or(&"").chars().count() + 1;

        let start_index = if pos >= 80 { pos - 80 } else { 0 };

        let end_index = if pos > source.len() {
            source.len()
        } else {
            pos
        };

        Self {
            line: line_count,
            col: last_line_len,
            _snippet: source[start_index..end_index].trim().to_string(),
        }
    }
}
