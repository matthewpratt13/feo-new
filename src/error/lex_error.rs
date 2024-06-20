use core::fmt;
use std::error::Error;

/// Enum representing the different types of lexer errors.
/// Used in conjunction with `CompilerError` to keep track of errors encountered
/// during tokenization.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexErrorKind {
    CharNotFound {
        expected: String,
    },

    EmptyCharLiteral,

    InvalidHashLength {
        len: usize,
    },

    MismatchedDelimiter {
        expected: char,
        found: char,
    },

    MissingDelimiter {
        delim: char,
    },

    MissingQuote {
        quote: char,
    },

    ParseIntError,
    ParseUIntError,
    ParseBigUIntError,
    ParseFloatError,
    ParseBoolError,

    UnexpectedBigUIntSuffix {
        suffix: String,
    },

    UnexpectedChar {
        expected: String,
        found: char,
    },

    UnexpectedHashOrBigUIntPrefix {
        prefix: char,
    },

    UnexpectedHashSuffix {
        suffix: String,
    },

    UnexpectedNumericSuffix {
        suffix: String,
    },

    UnrecognizedChar {
        value: String,
    },

    UnrecognizedEscapeSequence {
        sequence: char,
    },

    UnrecognizedInnerAttribute {
        name: String,
    },

    UnrecognizedKeyword {
        name: String,
    },

    UnrecognizedOuterAttribute {
        name: String,
    },

    UnexpectedEndOfInput,

    #[default]
    UnknownError,
}

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexErrorKind::CharNotFound { expected } => {
                write!(f, "character not found: expected {expected}, found none")
            }
            LexErrorKind::EmptyCharLiteral => {
                write!(f, "scanning error: empty character literal")
            }
            LexErrorKind::InvalidHashLength { len } => {
                write!(f, "syntax error: invalid hash length – {len}")
            }
            LexErrorKind::MismatchedDelimiter { expected, found } => write!(
                f,
                "mismatched delimiter: expected `{expected}`, found `{found}`"
            ),
            LexErrorKind::MissingDelimiter { delim } => {
                write!(f, "missing delimiter: expected `{delim}`, found none")
            }
            LexErrorKind::MissingQuote { quote } => {
                write!(f, "missing quote: expected `{quote}`, found none")
            }
            LexErrorKind::ParseIntError => write!(f, "error parsing signed integer literal"),
            LexErrorKind::ParseUIntError => write!(f, "error parsing unsigned integer literal"),
            LexErrorKind::ParseBigUIntError => {
                write!(f, "error parsing large unsigned integer literal")
            }
            LexErrorKind::ParseFloatError => {
                write!(f, "error parsing floating-point number literal")
            }
            LexErrorKind::ParseBoolError => write!(f, "error parsing boolean literal"),
            LexErrorKind::UnexpectedChar { expected, found } => write!(
                f,
                "unexpected character: expected {expected}, found `{found}`",
            ),
            LexErrorKind::UnexpectedEndOfInput => {
                write!(f, "scanning error: unexpected end of input")
            }
            LexErrorKind::UnexpectedBigUIntSuffix { suffix } => {
                write!(
                    f,
                    "unexpected large unsigned integer suffix: expected `u256` or `u512`, found {suffix}"
                )
            }
            LexErrorKind::UnexpectedHashSuffix { suffix } => {
                write!(
                    f,
                    "unexpected hash suffix: expected `h160`, `h256` or `u512`; found {suffix}"
                )
            }
            LexErrorKind::UnexpectedHashOrBigUIntPrefix { prefix } => {
                write!(
                    f,
                    "unexpected hexadecimal prefix: expected `0x`, found {prefix}"
                )
            }
            LexErrorKind::UnexpectedNumericSuffix { suffix } => {
                write!(f, "unexpected numeric suffix: expected `i32`, `i64`, `u8`, `u16`, `u32` `u64`, `f32` or `f64`; found {suffix}")
            }
            LexErrorKind::UnrecognizedChar { value } => {
                write!(f, "syntax error: unrecognized character – `{value}`")
            }
            LexErrorKind::UnrecognizedEscapeSequence { sequence } => {
                write!(
                    f,
                    "syntax error: unrecognized escape sequence – `{sequence}`"
                )
            }
            LexErrorKind::UnrecognizedInnerAttribute { name } => {
                write!(f, "syntax error: unrecognized inner attribute – `{name}`")
            }
            LexErrorKind::UnrecognizedKeyword { name } => {
                write!(f, "syntax error: unrecognized keyword – `{name}`")
            }
            LexErrorKind::UnrecognizedOuterAttribute { name } => {
                write!(f, "syntax error: unrecognized outer attribute – `{name}`")
            }
            LexErrorKind::UnknownError => write!(f, "unknown lexer error"),
        }
    }
}

impl Error for LexErrorKind {}
