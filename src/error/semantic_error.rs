#![allow(dead_code)]

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
        found: String,
    },

    TypeMismatchBinaryExpr {
        expected: String,
        found: String,
    },

    UndefinedVariable {
        name: Identifier,
    },

    UnexpectedType {
        expected: String,
        found: String,
    },

    InvalidPathIdentifier {
        name: Identifier,
    },

    UndefinedPath {
        name: Identifier,
    },

    ConversionError {
        from: String,
        into: String,
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
            SemanticErrorKind::TypeMismatchBinaryExpr { expected, found } => write!(
                f,
                "type mismatch in binary expression. Expected `{expected}`, found: `{found}`"
            ),

            SemanticErrorKind::UnexpectedType { expected, found } => {
                write!(f, "unexpected type. Expected {expected}, found {found}")
            }

            SemanticErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable: {name}",)
            }
            SemanticErrorKind::InvalidPathIdentifier { name } => {
                write!(f, "invalid path identifier: {name}")
            }
            SemanticErrorKind::UndefinedPath { name } => write!(f, "undefined path: {name}"),

            SemanticErrorKind::ConversionError { from, into } => {
                write!(f, "conversion error. Unable to convert {from} into {into}")
            }

            SemanticErrorKind::UnknownError => write!(f, "unknown semantic analysis error"),
        }
    }
}

impl Error for SemanticErrorKind {}
