use crate::{span::Position, token::Token};

use core::fmt;
use std::error::Error;

/// Enum representing the different types of parsing errors.
/// Used in conjunction with `CompilerError` to keep track of errors encountered
/// during parsing.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum ParserErrorKind {
    StrDecodeError(core::char::DecodeUtf16Error),

    UnexpectedEndOfInput,

    UnexpectedToken {
        expected: String,
        found: Option<Token>,
    },

    MissingToken {
        expected: String,
    },

    UnmatchedDelimiter {
        delim: String,
        position: Position,
    },

    InvalidTokenContext {
        token: Option<Token>,
    },

    InvalidTypeParameter {
        expected: String,
        found: String,
    },

    ExtraTokens {
        token: Option<Token>,
        msg: String,
    },

    UnexpectedExpression {
        expected: String,
        found: String,
    },

    MissingExpression {
        expected: String,
    },

    MissingItem {
        expected: String,
    },

    MissingType {
        expected: String,
    },

    MissingPattern {
        expected: String,
    },

    UndeclaredGenerics {
        found: String,
    },

    UnexpectedRangeOp {
        expected: String,
        found: String,
    },

    #[default]
    UnknownError,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserErrorKind::StrDecodeError(e) => {
                write!(f, "error decoding u16 to `Str` type: {e}")
            }
            ParserErrorKind::UnexpectedEndOfInput => {
                write!(f, "unexpected end of input")
            }
            ParserErrorKind::UnexpectedToken { expected, found } => write!(
                f,
                "unexpected token. Expected {expected}, found `{:?}`",
                found
            ),
            ParserErrorKind::MissingToken { expected } => {
                write!(f, "token not found. Expected {expected}, found none")
            }
            ParserErrorKind::UnmatchedDelimiter { delim, position } => {
                write!(
                    f,
                    "unmatched `{delim}` delimiter [Ln {}, Col {}]. Expected matching delimiter",
                    position.line, position.col
                )
            }
            ParserErrorKind::InvalidTokenContext { token } => {
                write!(
                    f,
                    "syntax error. Token invalid in current context: `{:?}`",
                    token
                )
            }
            ParserErrorKind::InvalidTypeParameter { expected, found } => {
                write!(
                    f,
                    "invalid type parameter. Expected {expected}, found {found}"
                )
            }
            ParserErrorKind::ExtraTokens { token, msg } => {
                write!(
                    f,
                    "syntax error. Detected extra tokens: `{:?}`. {msg}",
                    token
                )
            }
            ParserErrorKind::UnexpectedExpression { expected, found } => {
                write!(
                    f,
                    "unexpected expression. Expected {expected}, found {found}"
                )
            }
            ParserErrorKind::MissingExpression { expected } => {
                write!(f, "expression not found. Expected {expected}, found none")
            }
            ParserErrorKind::MissingItem { expected } => {
                write!(f, "item not found. Expected {expected}, found none")
            }
            ParserErrorKind::MissingType { expected } => {
                write!(
                    f,
                    "type annotation not found. Expected {expected}, found none"
                )
            }
            ParserErrorKind::MissingPattern { expected } => {
                write!(f, "pattern not found. Expected {expected}, found none")
            }
            ParserErrorKind::UndeclaredGenerics { found } => {
                write!(f, "undeclared generic annotation ({found}). Generics must be declared after `impl` (e.g., `impl<T>`")
            }
            ParserErrorKind::UnexpectedRangeOp { expected, found } => write!(
                f,
                "unexpected range operator. Expected {expected}, found {found}"
            ),
            ParserErrorKind::UnknownError => write!(f, "unknown parsing error"),
        }
    }
}

impl Error for ParserErrorKind {}
