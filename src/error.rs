#![allow(dead_code)]

use std::{error::Error, fmt, sync::Arc};

use crate::token::Token;

/// Enum representing the different types of lexer errors.
#[derive(Default, Debug, PartialEq)]
pub enum LexerErrorKind {
    ParseHexError,
    ParseIntError,
    ParseUIntError,

    UnexpectedCharacter {
        expected: String,
        found: char,
    },

    MissingQuote {
        quote: char,
    },

    MissingDelimiter {
        delim: char,
    },

    MismatchedDelimiter {
        expected: char,
        found: char,
    },

    InvalidEscapeSequence {
        sequence: char,
    },

    CharacterNotFound {
        expected: String,
    },

    #[default]
    UnknownError,
    // TODO: add more error types as needed
}

impl fmt::Display for LexerErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexerErrorKind::ParseHexError => write!(f, "error parsing hexadecimal digit"),
            LexerErrorKind::ParseIntError => write!(f, "error parsing signed integer"),
            LexerErrorKind::ParseUIntError => write!(f, "error parsing unsigned integer"),
            LexerErrorKind::UnexpectedCharacter { expected, found } => writeln!(
                f,
                "unexpected character\nexpected {expected}, found `{found}`",
            ),
            LexerErrorKind::MissingQuote { quote } => writeln!(f, "expected `{quote}`, found none"),

            LexerErrorKind::MissingDelimiter { delim } => {
                writeln!(f, "expected `{delim}`, found none")
            }
            LexerErrorKind::MismatchedDelimiter { expected, found } => writeln!(
                f,
                "mismatched delimiter\nexpected `{expected}`, found `{found}`"
            ),
            LexerErrorKind::InvalidEscapeSequence { sequence } => {
                writeln!(f, "detected invalid escape sequence: `{sequence}`")
            }
            LexerErrorKind::CharacterNotFound { expected } => {
                writeln!(f, "expected {expected}, found none")
            }
            LexerErrorKind::UnknownError => writeln!(f, "unknown error"),
        }
    }
}

impl Error for LexerErrorKind {}

/// Enum representing the different types of parsing errors.
#[derive(Default, Debug, PartialEq)]
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
                "syntax error: unexpected token\nexpected {}, found `{:#?}`",
                expected, found
            ),
            ParserErrorKind::UnexpectedEndOfInput => writeln!(f, "unexpected end of input"),
            ParserErrorKind::InvalidToken { token } => writeln!(
                f,
                "syntax error: invalid token in current context (`{:#?}`)",
                token
            ),
            ParserErrorKind::TokenNotFound => {
                writeln!(f, "expected token, found none")
            }
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

/// Generic error struct that allows for custom error kinds, and provides precise locations.
#[derive(Debug, Clone)]
pub struct CompilerError<T> {
    error_kind: T,
    line: usize,
    col: usize,
    source: Arc<String>,
}

impl<T> CompilerError<T> {
    /// Constructor method
    pub fn new(source: &str, pos: usize, error_kind: T) -> Self {
        let slice = &source[..pos];
        let lines = slice.split('\n').collect::<Vec<_>>();
        let line_count = lines.len();
        let last_line_len = lines.last().unwrap_or(&"").chars().count() + 1;

        Self {
            error_kind,
            line: line_count,
            col: last_line_len,
            source: Arc::new(source.to_string()),
        }
    }
}

impl<T> fmt::Display for CompilerError<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} [Ln {}, Col{}]", self.error_kind, self.line, self.col)
    }
}

impl<T> Error for CompilerError<T> where T: fmt::Display + fmt::Debug {}

/// Dummy struct that has no real functionality of its own.
/// Used as a placeholder for some `Err` in functions that return a `Result`, to prove
/// that an error has occurred without returning the actual error, instead allowing the error
/// to be logged in the respective struct for later use.
pub struct ErrorEmitted(pub ());
