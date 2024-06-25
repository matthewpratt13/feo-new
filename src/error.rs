mod lex_error;
mod parser_error;
mod semantic_error;

use std::{error::Error, fmt};

use crate::span::Position;

pub(crate) use self::lex_error::LexErrorKind;
pub(crate) use self::parser_error::ParserErrorKind;
pub(crate) use self::semantic_error::SemanticErrorKind;

/// Generic error struct that encapsulates custom error kinds and provides the precise location
/// of the error in the source code.
#[derive(Debug, Clone)]
pub struct CompilerError<T: Clone> {
    error_kind: T,
    position: Position,
}

impl<T> CompilerError<T>
where
    T: Clone,
{
    /// Create a new `CompilerError` that provides details at a precise location in the source code.
    pub(crate) fn new(error_kind: T, pos: usize, source: &str) -> Self {
        CompilerError {
            error_kind,
            position: Position::new(pos, source),
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
            "{} [error position: Ln {}, Col {}]",
            self.error_kind, self.position.line, self.position.col
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
