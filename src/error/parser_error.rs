use std::{error::Error, fmt};

use crate::token::Token;

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

    UnexpectedExpression {
        expected: String,
        found: String,
    },

    // TODO: add `MissingExpression`

    // TODO: add `UnexpectedPattern`

    // TODO: add `MissingItems`

    // TODO: add `UnexpectedItem`
    #[default]
    UnknownError,
}

impl fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserErrorKind::UnexpectedToken { expected, found } => write!(
                f,
                "unexpected token. Expected {}, found `{:?}`",
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
                write!(f, "syntax error. Invalid token context – `{:?}`", token)
            }

            ParserErrorKind::ExtraTokens { token, msg } => {
                write!(
                    f,
                    "syntax error. Extra tokens detected – `{:?}`. {msg}",
                    token
                )
            }

            ParserErrorKind::UnexpectedExpression { expected, found } => {
                write!(
                    f,
                    "unexpected expression. Expected {expected}, found {found}"
                )
            }

            ParserErrorKind::UnknownError => write!(f, "unknown parsing error"),
        }
    }
}

impl Error for ParserErrorKind {}
