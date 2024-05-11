use std::{error::Error, fmt};

use crate::token::Token;

/// Enum representing the different types of parsing errors.
/// Used in conjunction with `CompilerError` to keep track of errors encountered
/// during parsing.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum ParserErrorKind {
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
    },

    InvalidTokenContext {
        token: Option<Token>,
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

    UnexpectedPattern {
        expected: String,
        found: String,
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

    #[default]
    UnknownError,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserErrorKind::UnexpectedEndOfInput => {
                write!(f, "unexpected end of input")
            }
            ParserErrorKind::UnexpectedToken { expected, found } => write!(
                f,
                "unexpected token. Expected {}, found `{:?}`",
                expected, found
            ),
            ParserErrorKind::MissingToken { expected } => {
                write!(f, "token not found. Expected {expected}, found none")
            }
            ParserErrorKind::UnmatchedDelimiter { delim } => {
                write!(
                    f,
                    "unmatched delimiter. Expected delimiter to match `{delim}`"
                )
            }
            ParserErrorKind::InvalidTokenContext { token } => {
                write!(
                    f,
                    "syntax error. Token invalid in current context: `{:?}`",
                    token
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
            ParserErrorKind::UnexpectedPattern { expected, found } => {
                write!(f, "unexpected pattern. Expected {expected}, found {found}")
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
            ParserErrorKind::UnknownError => write!(f, "unknown parsing error"),
        }
    }
}

impl Error for ParserErrorKind {}
