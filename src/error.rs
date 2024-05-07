use std::{error::Error, fmt, sync::Arc};

use crate::token::Token;

/// Enum representing the different types of lexer errors.
/// Used in conjunction with `CompilerError` to keep track of errors encountered
/// during tokenization.
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
            LexErrorKind::LexIntError => write!(f, "error tokenizing signed integer literal"),
            LexErrorKind::LexUIntError => write!(f, "error tokenizing unsigned integer literal"),
            LexErrorKind::LexBigUIntError => {
                write!(f, "error tokenizing large unsigned integer literal")
            }
            LexErrorKind::LexBoolError => write!(f, "error tokenizing boolean literal"),
            LexErrorKind::LexHashError => write!(f, "error tokenizing hash literal"),
            LexErrorKind::InvalidHashLength { len } => {
                write!(f, "syntax error: invalid hash length – {len}")
            }
            LexErrorKind::UnrecognizedChar { value } => {
                write!(f, "syntax error: unrecognized character – `{value}`")
            }
            LexErrorKind::EmptyCharLiteral => {
                write!(f, "scanning error: empty character literal")
            }
            LexErrorKind::UnrecognizedKeyword { name } => {
                write!(f, "syntax error: unrecognized keyword – `{name}`")
            }
            LexErrorKind::UnrecognizedInnerAttribute { name } => {
                write!(f, "syntax error: unrecognized inner attribute – `{name}`")
            }
            LexErrorKind::UnrecognizedOuterAttribute { name } => {
                write!(f, "syntax error: unrecognized outer attribute – `{name}`")
            }
            LexErrorKind::UnexpectedChar { expected, found } => write!(
                f,
                "unexpected character: expected {expected}, found `{found}`",
            ),
            LexErrorKind::MissingQuote { quote } => {
                write!(f, "missing quote: expected `{quote}`, found none")
            }
            LexErrorKind::MissingDelimiter { delim } => {
                write!(f, "missing delimiter: expected `{delim}`, found none")
            }
            LexErrorKind::MismatchedDelimiter { expected, found } => write!(
                f,
                "mismatched delimiter: expected `{expected}`, found `{found}`"
            ),
            LexErrorKind::UnrecognizedEscapeSequence { sequence } => {
                write!(
                    f,
                    "syntax error: unrecognized escape sequence – `{sequence}`"
                )
            }
            LexErrorKind::CharNotFound { expected } => {
                write!(f, "character not found: expected {expected}, found none")
            }
            LexErrorKind::UnexpectedEndOfInput => {
                write!(f, "scanning error: unexpected end of input")
            }
            LexErrorKind::UnknownError => write!(f, "unknown lexer error"),
        }
    }
}

impl Error for LexErrorKind {}

/// Enum representing the different types of parsing errors.
/// Used in conjunction with `CompilerError` to keep track of errors encountered
/// during parsing.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum ParserErrorKind {
    UnexpectedToken {
        expected: String,
        found: Option<Token>,
    },

    UnexpectedEndOfInput,

    MissingToken {
        expected: String,
    },

    UnmatchedDelimiter {
        delim: String,
    },

    InvalidTokenContext {
        token: Option<Token>,
    },

    ExtraTokens {
        token: Option<Token>,
        msg: String,
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
            ParserErrorKind::UnexpectedToken { expected, found } => write!(
                f,
                "unexpected token. Expected {}, found `{:#?}`",
                expected, found
            ),
            ParserErrorKind::UnexpectedEndOfInput => {
                write!(f, "parsing error. Unexpected end of input")
            }
            ParserErrorKind::MissingToken { expected } => {
                write!(f, "token not found. Expected {expected}, found none")
            }
            ParserErrorKind::UnmatchedDelimiter { delim } => {
                write!(f, "unmatched delimiter ({delim})")
            }

            ParserErrorKind::InvalidTokenContext { token } => {
                write!(f, "syntax error. Invalid token context – `{:#?}`", token)
            }

            ParserErrorKind::ExtraTokens { token, msg } => {
                write!(
                    f,
                    "syntax error. Extra tokens detected – `{:#?}`. {msg}",
                    token
                )
            }

            ParserErrorKind::TypeConversionError { type_a, type_b } => write!(
                f,
                "conversion error. Unable to convert {type_a} into {type_b}"
            ),

            ParserErrorKind::UnknownError => write!(f, "unknown parsing error"),
        }
    }
}

impl Error for ParserErrorKind {}

/// Generic error struct that encapsulates custom error kinds, and provides the precise location
/// of the error in the source code.
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
    pub(crate) fn new(error_kind: T, source: &str, pos: usize) -> Self {
        if pos > source.len() {
            panic!("position out of bounds");
        }

        let slice = &source[..pos];
        let lines: Vec<&str> = slice.split('\n').collect();
        let line_count = lines.len();
        let last_line_len = lines.last().unwrap_or(&"").chars().count() + 1;

        let start_pos = if pos > 80 { pos - 80 } else { 0 };

        Self {
            error_kind,
            line: line_count,
            col: last_line_len,
            _source: Arc::new(source[start_pos..pos].trim().to_string()),
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
            "ERROR: {} [Ln {}, Col {}]",
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
pub(crate) struct ErrorsEmitted;
