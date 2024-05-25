use core::fmt;
use std::error::Error;

use crate::ast::Identifier;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    DuplicateVariable {
        name: Identifier,
    },

    TypeMismatch {
        expected: String,
        found: Identifier,
    },

    UndefinedVariable {
        name: Identifier,
    },

    InvalidPathIdentifier {
        name: Identifier,
    },

    UndefinedPath {
        name: Identifier,
    },

    #[default]
    UnknownError,
}

impl fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticErrorKind::DuplicateVariable { name } => {
                write!(f, "duplicate variable: `{name}`")
            }
            SemanticErrorKind::TypeMismatch { expected, found } => {
                write!(f, "type mismatch. Expected {expected}, found {found}")
            }
            SemanticErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable: {name}",)
            }
            SemanticErrorKind::InvalidPathIdentifier { name } => {
                write!(f, "invalid path identifier: {name}")
            }
            SemanticErrorKind::UndefinedPath { name } => write!(f, "undefined path: {name}"),

            SemanticErrorKind::UnknownError => write!(f, "unknown semantic analysis error"),
        }
    }
}

impl Error for SemanticErrorKind {}
