#![allow(dead_code)]

use std::{error::Error, fmt, sync::Arc};

use crate::token::Token;

// TODO: implement `fmt::Display` (and `std::error::Error`?)
/// Enum  representing the different types of lexing error variants.
#[derive(Default, Debug, PartialEq)]
pub enum LexerErrorKind {
    UnexpectedCharacter(char),
    UnclosedStringLiteral,
    MismatchedDelimiter(char),
    InvalidEscapeSequenceInStringLiteral,

    #[default]
    UnknownError,
    // TODO: add more error types as needed
}

/// Lexer error struct that contains an error kind and the current position in the source code.
#[derive(Debug, PartialEq)]
pub struct LexerError {
    pub error_kind: LexerErrorKind,
    pub pos: usize,
}

/// Enum representing the different types of parsing error variants.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum ParserErrorKind {
    UnexpectedToken {
        expected: String,
        found: Token,
    },

    UnexpectedEndOfInput,

    InvalidToken {
        token: Token,
    },

    TokenIndexOutOfBounds {
        len: usize,
        i: usize,
    },

    TokenNotFound,

    #[default]
    UnknownError,
    // TODO: add more error types as needed
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserErrorKind::UnexpectedToken { expected, found } => writeln!(
                f,
                "unexpected token \nexpected: {}, found `{:#?}`",
                expected, found
            ),
            ParserErrorKind::UnexpectedEndOfInput => writeln!(f, "unexpected end of input"),
            ParserErrorKind::InvalidToken { token } => writeln!(f, "invalid token: `{:#?}`", token),
            ParserErrorKind::TokenNotFound => writeln!(f, "token not found"),
            ParserErrorKind::TokenIndexOutOfBounds { len, i } => {
                writeln!(
                    f,
                    "token index out of bounds. \nlength is: {len}, index is: {i}"
                )
            }
            ParserErrorKind::UnknownError => writeln!(f, "unknown error"),
        }
    }
}

impl Error for ParserErrorKind {}

/// Parser error struct contains the error kind, and its position in the source code.
#[derive(Debug)]
pub struct ParserError {
    error_kind: ParserErrorKind,
    line: usize,
    col: usize,
    source: Arc<String>,
}

impl ParserError {
    /// Constructor method.
    /// Retrieve the line and column position within the source code and include the error kind.
    pub fn new(source: &str, pos: usize, error_kind: ParserErrorKind) -> Self {
        let slice = &source[..pos];
        let lines = slice.split('\n').collect::<Vec<_>>();
        let line_count = lines.len();
        let last_line_len = lines.last().unwrap_or(&"").chars().count() + 1;

        ParserError {
            error_kind,
            line: line_count,
            col: last_line_len,
            source: Arc::new(source.to_string()),
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} [Ln {}, Col{}]", self.error_kind, self.line, self.col)
    }
}

impl Error for ParserError {}
