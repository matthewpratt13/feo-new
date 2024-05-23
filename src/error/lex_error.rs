use core::fmt;
use std::error::Error;

/// Enum representing the different types of lexer errors.
/// Used in conjunction with `CompilerError` to keep track of errors encountered
/// during tokenization.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexErrorKind {
    LexIntError,
    LexUIntError,
    LexBigUIntError,
    LexFloatError,
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
            LexErrorKind::LexFloatError => {
                write!(f, "error tokenizing floating-point number literal")
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
