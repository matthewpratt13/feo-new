#![allow(dead_code)]

use std::fmt;

use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum LexerErrorKind {
    UnexpectedCharacter(char),
    UnclosedStringLiteral,
    MismatchedDelimiter(char),
    // Add more error types as needed
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

    TokenNotFound,

    #[default]
    UnknownError,
    // Add more error types as needed
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

// Implement methods for ParseErrorContext
impl<'a> ParseErrorContext<'a> {
    // Constructor method
    fn new(line: usize, col: usize, msg: String, source: &'a str) -> Self {
        ParseErrorContext {
            line,
            col,
            msg,
            source,
        }
    }

    // Method to format error message
    fn format_error_message(&self) -> String {
        format!(
            "Error at line {}, column {}: {}\nSSource: {}",
            self.line, self.col, self.msg, self.source
        )
    }
}
