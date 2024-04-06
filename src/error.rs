use std::{error::Error, fmt, sync::Arc};

use crate::token::Token;

/// Enum representing the different types of lexer errors.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexErrorKind<'a> {
    ParseHexError,
    ParseIntError,
    ParseUIntError,
    ParseHashError,
    ParseAddressError,
    ParseBoolError,

    EmptyCharLiteral,

    UnrecognizedChar {
        value: &'a str,
    },

    UnexpectedChar {
        expected: &'a str,
        found: char,
    },

    CharNotFound {
        expected: &'a str,
    },

    AtSignReserved,

    DollarSignReserved,

    UnrecognizedKeyword {
        name: &'a str,
    },

    UnrecognizedAttribute {
        name: &'a str,
    },

    MissingDelimiter {
        delim: char,
    },

    MismatchedDelimiter {
        expected: char,
        found: char,
    },

    MissingQuote {
        quote: char,
    },

    UnrecognizedEscapeSequence {
        sequence: char,
    },

    UnexpectedEndOfInput,

    #[default]
    UnknownError,
}

impl<'a> fmt::Display for LexErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexErrorKind::ParseHexError => writeln!(f, "error parsing hexadecimal digit"),
            LexErrorKind::ParseIntError => writeln!(f, "error parsing signed integer"),
            LexErrorKind::ParseUIntError => writeln!(f, "error parsing unsigned integer"),
            LexErrorKind::ParseHashError => writeln!(f, "error parsing hash"),
            LexErrorKind::ParseAddressError => writeln!(f, "error parsing address"),
            LexErrorKind::ParseBoolError => writeln!(f, "error parsing boolean"),

            LexErrorKind::UnrecognizedChar { value } => {
                writeln!(f, "syntax error: unrecognized character – `{value}`")
            }
            LexErrorKind::EmptyCharLiteral => {
                writeln!(f, "scanning error: empty character literal")
            }
            LexErrorKind::UnrecognizedKeyword { name } => {
                writeln!(f, "syntax error: unrecognized keyword – `{name}`")
            }
            LexErrorKind::UnrecognizedAttribute { name } => {
                writeln!(f, "syntax error: unrecognized attribute – `{name}`")
            }

            LexErrorKind::AtSignReserved => {
                writeln!(f, "syntax error\n`@` is reserved for address literals")
            }
            LexErrorKind::DollarSignReserved => {
                writeln!(f, "syntax error\n`$` is reserved for hash literals")
            }

            LexErrorKind::UnexpectedChar { expected, found } => writeln!(
                f,
                "unexpected character: expected {expected}, found `{found}`",
            ),
            LexErrorKind::MissingQuote { quote } => {
                writeln!(f, "missing quote: expected `{quote}`, found none")
            }

            LexErrorKind::MissingDelimiter { delim } => {
                writeln!(f, "missing delimiter: expected `{delim}`, found none")
            }
            LexErrorKind::MismatchedDelimiter { expected, found } => writeln!(
                f,
                "mismatched delimiter: expected `{expected}`, found `{found}`"
            ),
            LexErrorKind::UnrecognizedEscapeSequence { sequence } => {
                writeln!(
                    f,
                    "syntax error: unrecognized escape sequence – `{sequence}`"
                )
            }
            LexErrorKind::CharNotFound { expected } => {
                writeln!(f, "character not found: expected {expected}, found none")
            }
            LexErrorKind::UnexpectedEndOfInput => {
                writeln!(f, "scanning error: unexpected end of input")
            }
            LexErrorKind::UnknownError => writeln!(f, "unknown lexer error"),
        }
    }
}

impl<'a> Error for LexErrorKind<'a> {}

/// Enum representing the different types of parsing errors.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum ParserErrorKind<'a> {
    UnexpectedToken {
        expected: &'a str,
        found: Token<'a>,
    },

    InvalidToken {
        token: Token<'a>,
    },

    UnexpectedEndOfInput,

    TokenIndexOutOfBounds {
        len: usize,
        i: usize,
    },

    TokenNotFound,

    #[default]
    UnknownError,
}

impl<'a> fmt::Display for ParserErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserErrorKind::UnexpectedToken { expected, found } => writeln!(
                f,
                "unexpected token: expected {}, found `{:#?}`",
                expected, found
            ),
            ParserErrorKind::UnexpectedEndOfInput => {
                writeln!(f, "scanning: unexpected end of input")
            }
            ParserErrorKind::InvalidToken { token } => writeln!(
                f,
                "parsing error: invalid token in current context – `{:#?}`)",
                token
            ),
            ParserErrorKind::TokenNotFound => {
                writeln!(f, "expected token, found none")
            }
            ParserErrorKind::TokenIndexOutOfBounds { len, i } => {
                writeln!(
                    f,
                    "token index out of bounds: length is {len}, index is {i}"
                )
            }
            ParserErrorKind::UnknownError => writeln!(f, "unknown parser error"),
        }
    }
}

impl<'a> Error for ParserErrorKind<'a> {}

/// Generic error struct that allows for custom error kinds, and provides the precise location
/// of an error in the source code.
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
    /// Create a new `CompilerError` with precise locations in the sources.
    pub fn new(error_kind: T, source: &str, pos: usize) -> Self {
        let slice = &source[..pos];
        let lines: Vec<&str> = slice.split('\n').collect();
        let line_count = lines.len();
        let last_line_len = lines.last().unwrap_or(&"").chars().count() + 1;

        Self {
            error_kind,
            line: line_count,
            col: last_line_len,
            _source: Arc::new(source.to_string()),
        }
    }
}

impl<T> fmt::Display for CompilerError<T>
where
    T: Clone + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{} [Ln {}, Col {}]",
            self.error_kind, self.line, self.col
        )
    }
}

impl<T> Error for CompilerError<T> where T: Clone + fmt::Display + fmt::Debug {}

/// Dummy struct that has no real functionality of its own.
/// Used as a placeholder for some `Err` in functions that return a `Result`, to prove
/// that an error has occurred without returning the actual error, instead allowing the error
/// to be logged in the respective struct for later use.
#[derive(Debug)]
pub struct ErrorEmitted(pub ());
