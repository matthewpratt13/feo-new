#![allow(dead_code)]

use std::{error::Error, fmt, sync::Arc};

use crate::token::Token;

#[derive(Debug, Clone)]
pub struct CompilerError<T> {
    error_kind: T,
    line: usize,
    col: usize,
    source: Arc<String>,
}

impl<T> CompilerError<T> {
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

impl fmt::Display for LexerErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            _ => todo!(),
        }
    }
}

impl Error for LexerErrorKind {}

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

impl<T> fmt::Display for CompilerError<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} [Ln {}, Col{}]", self.error_kind, self.line, self.col)
    }
}

impl<T> Error for CompilerError<T> where T: fmt::Display + fmt::Debug {}

pub struct ErrorEmitted(pub ());
