use std::{error::Error, fmt, sync::Arc};

use crate::token::Token;

/// Enum representing the different types of lexer errors.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexErrorKind {
    ParseHexError,
    ParseIntError,
    ParseUIntError,
    ParseBoolError,
    ParsePuncError,

    InvalidPunc {
        punc: String,
    },

    UnrecognizedChar {
        value: char,
    },

    InvalidKeyword {
        name: String,
    },

    UnexpectedChar {
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

    CharNotFound {
        expected: String,
    },

    #[default]
    UnknownError,
    // TODO: add more error types as needed
}

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexErrorKind::ParseHexError => write!(f, "error parsing hexadecimal digit"),
            LexErrorKind::ParseIntError => write!(f, "error parsing signed integer"),
            LexErrorKind::ParseUIntError => write!(f, "error parsing unsigned integer"),
            LexErrorKind::ParseBoolError => write!(f, "error parsing boolean"),
            LexErrorKind::ParsePuncError => write!(f, "error parsing punctuation"),
            LexErrorKind::InvalidPunc { punc } => {
                write!(f, "syntax error\ninvalid punctuation: `{punc}`")
            }
            LexErrorKind::UnrecognizedChar { value } => {
                write!(f, "syntax error\nunrecognized character: `{value}`")
            }
            LexErrorKind::InvalidKeyword { name } => {
                writeln!(f, "syntax error\ninvalid keyword: `{name}`")
            }
            LexErrorKind::UnexpectedChar { expected, found } => writeln!(
                f,
                "unexpected character\nexpected {expected}, found `{found}`",
            ),
            LexErrorKind::MissingQuote { quote } => writeln!(f, "expected `{quote}`, found none"),

            LexErrorKind::MissingDelimiter { delim } => {
                writeln!(f, "expected `{delim}`, found none")
            }
            LexErrorKind::MismatchedDelimiter { expected, found } => writeln!(
                f,
                "mismatched delimiter\nexpected `{expected}`, found `{found}`"
            ),
            LexErrorKind::InvalidEscapeSequence { sequence } => {
                writeln!(f, "detected invalid escape sequence: `{sequence}`")
            }
            LexErrorKind::CharNotFound { expected } => {
                writeln!(f, "expected {expected}, found none")
            }
            LexErrorKind::UnknownError => writeln!(f, "unknown error"),
        }
    }
}

impl Error for LexErrorKind {}

/// Enum representing the different types of parsing errors.
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
    /// Constructor method
    pub fn new(source: &str, pos: usize, error_kind: T) -> Self {
        let slice = &source[..pos];
        let lines = slice.split('\n').collect::<Vec<&str>>();
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
        write!(
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
