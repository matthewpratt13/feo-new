mod lex_error;
pub(crate) use lex_error::LexErrorKind;

mod parser_error;
pub(crate) use parser_error::ParserErrorKind;

mod semantic_error;
pub(crate) use semantic_error::SemanticErrorKind;

use crate::span::Position;

use core::fmt;
use std::error::Error;

/// Generic error struct that encapsulates custom error kinds and provides the precise location
/// of the error in the source code.
#[derive(Debug, Clone)]
pub struct CompilerError<T: Clone + fmt::Debug> {
    error_kind: T,
    position: Position,
}

impl<T> CompilerError<T>
where
    T: Clone + fmt::Debug,
{
    /// Create a new `CompilerError` that provides details at a precise location in the source code.
    pub(crate) fn new(error_kind: T, pos: usize, source: &str) -> Self {
        CompilerError {
            error_kind,
            position: Position::new(pos, source),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn error_kind(&self) -> T {
        self.error_kind.clone()
    }
}

impl<T> fmt::Display for CompilerError<T>
where
    T: Clone + fmt::Display + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [error position: Ln {}, Col {}]",
            self.error_kind, self.position.line, self.position.col
        )
    }
}

impl<T> Error for CompilerError<T>
where
    T: Clone + fmt::Display + fmt::Debug + Error + 'static,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.error_kind)
    }
}

/// Dummy struct that has no real functionality of its own.
/// Used as a placeholder for some `Err` in functions that return a `Result`, to prove
/// that an error has occurred without returning the actual error, instead allowing the error
/// to be logged in the respective struct for later use.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ErrorsEmitted;
