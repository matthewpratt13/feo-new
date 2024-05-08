mod lex_error;
mod parser_error;

use std::{error::Error, fmt, sync::Arc};

pub(crate) use self::lex_error::LexErrorKind;
pub(crate) use self::parser_error::ParserErrorKind;

/// Generic error struct that encapsulates custom error kinds, and provides the precise location
/// of the error in the source code.
#[derive(Debug, Clone)]
pub struct CompilerError<T: Clone> {
    error_kind: T,
    line: usize,
    col: usize,
    _source: Arc<String>,
}

impl<T> CompilerError<T>
where
    T: Clone,
{
    /// Create a new `CompilerError` that provides details at a precise location in the source code.
    pub(crate) fn new(error_kind: T, source: &str, pos: usize) -> Self {
        if pos > source.len() {
            panic!("position out of bounds");
        }

        let slice = &source[..pos];
        let lines: Vec<&str> = slice.split('\n').collect();
        let line_count = lines.len();
        let last_line_len = lines.last().unwrap_or(&"").chars().count() + 1;

        Self {
            error_kind,
            line: line_count,
            col: last_line_len,
            _source: Arc::new(source.trim().to_string()),
        }
    }
}

impl<T> fmt::Display for CompilerError<T>
where
    T: Clone + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ERROR: {} [Ln {}, Col {}]",
            self.error_kind, self.line, self.col
        )
    }
}

impl<T> Error for CompilerError<T> where T: Clone + fmt::Display + fmt::Debug {}

/// Dummy struct that has no real functionality of its own.
/// Used as a placeholder for some `Err` in functions that return a `Result`, to prove
/// that an error has occurred without returning the actual error, instead allowing the error
/// to be logged in the respective struct for later use.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ErrorsEmitted;
