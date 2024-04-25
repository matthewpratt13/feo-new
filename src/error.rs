use std::{error::Error, fmt, sync::Arc};

use crate::token::Token;

/// Enum representing the different types of lexer (scanner) errors.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexErrorKind {
    LexIntError,
    LexUIntError,
    LexBigUIntError,
    LexBoolError,
    LexHashError,

    InvalidHashLength {
        len: usize,
    },

    EmptyCharLiteral,

    UnrecognizedChar {
        value: String,
    },

    UnexpectedChar {
        expected: String,
        found: char,
    },

    CharNotFound {
        expected: String,
    },

    UnrecognizedKeyword {
        name: String,
    },

    UnrecognizedInnerAttribute {
        name: String,
    },

    UnrecognizedOuterAttribute {
        name: String,
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

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexErrorKind::LexIntError => writeln!(f, "error tokenizing signed integer literal"),
            LexErrorKind::LexUIntError => writeln!(f, "error tokenizing unsigned integer literal"),
            LexErrorKind::LexBigUIntError => {
                writeln!(f, "error tokenizing big unsigned integer literal")
            }
            LexErrorKind::LexBoolError => writeln!(f, "error tokenizing boolean literal"),
            LexErrorKind::LexHashError => writeln!(f, "error tokenizing hash literal"),
            LexErrorKind::InvalidHashLength { len } => {
                writeln!(f, "syntax error: invalid hash length – {len}")
            }
            LexErrorKind::UnrecognizedChar { value } => {
                writeln!(f, "syntax error: unrecognized character – `{value}`")
            }
            LexErrorKind::EmptyCharLiteral => {
                writeln!(f, "scanning error: empty character literal")
            }
            LexErrorKind::UnrecognizedKeyword { name } => {
                writeln!(f, "syntax error: unrecognized keyword – `{name}`")
            }
            LexErrorKind::UnrecognizedInnerAttribute { name } => {
                writeln!(f, "syntax error: unrecognized inner attribute – `{name}`")
            }
            LexErrorKind::UnrecognizedOuterAttribute { name } => {
                writeln!(f, "syntax error: unrecognized outer attribute – `{name}`")
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

impl Error for LexErrorKind {}

/// Enum representing the different types of parsing errors.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum ParserErrorKind {
    UnexpectedToken {
        expected: String,
        found: Option<Token>,
    },

    UnexpectedEndOfInput,

    TokenNotFound {
        expected: String,
    },

    MissingDelimiter {
        delim: char,
    },

    TypeConversionError {
        type_a: String,
        type_b: String,
    },

    #[default]
    UnknownError,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserErrorKind::UnexpectedToken { expected, found } => writeln!(
                f,
                "unexpected token: expected {}, found `{:#?}`",
                expected, found
            ),
            ParserErrorKind::UnexpectedEndOfInput => {
                writeln!(f, "parsing error: unexpected end of input")
            }
            ParserErrorKind::TokenNotFound { expected } => {
                writeln!(f, "token not found: expected {expected}, found none")
            }
            ParserErrorKind::MissingDelimiter { delim } => {
                writeln!(f, "missing delimiter: expected `{delim}`, found none")
            }
            ParserErrorKind::UnknownError => writeln!(f, "unknown parser error"),

            ParserErrorKind::TypeConversionError { type_a, type_b } => writeln!(
                f,
                "conversion error: unable to convert {type_a} into {type_b}"
            ),
        }
    }
}

impl Error for ParserErrorKind {}

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
    /// Create a new `CompilerError` that provides details at a precise location in the source code.
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
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorsEmitted;
