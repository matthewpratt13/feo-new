#![allow(dead_code)]

use std::fmt;

use crate::token::Token;

// TODO: implement `fmt::Display` (and `std::error::Error`?)
#[derive(Debug, PartialEq)]
pub enum LexerErrorKind {
    UnexpectedCharacter(char),
    UnclosedStringLiteral,
    MismatchedDelimiter(char),
    InvalidEscapeSequenceInStringLiteral, // TODO: add more error types as needed
}

#[derive(Debug, PartialEq)]
pub struct LexerError {
    pub error_kind: LexerErrorKind,
    pub pos: usize,
}

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

#[derive(Debug)]
pub struct ParseErrorContext<'a> {
    line: usize,
    col: usize,
    msg: String,
    source: &'a str,
}

impl<'a> ParseErrorContext<'a> {
    /// Constructor method
    pub fn new(line: usize, col: usize, msg: String, source: &'a str) -> Self {
        ParseErrorContext {
            line,
            col,
            msg,
            source,
        }
    }

    // TODO: integrate with `fmt()` and `ParserErrorKind` above
    /// Method to format error message
    fn format_error_message(&self) -> String {
        format!(
            "Error at line {}, column {}: {}\nSSource: {}",
            self.line, self.col, self.msg, self.source
        )
    }
}
