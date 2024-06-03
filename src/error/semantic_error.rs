#![allow(dead_code)]

use core::fmt;
use std::error::Error;

use crate::ast::Identifier;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    ConversionError {
        from: String,
        into: String,
    },

    DuplicateImport {
        name: Identifier,
    },

    DuplicateVariable {
        name: Identifier,
    },

    InvalidPathIdentifier {
        name: Identifier,
    },

    InvalidStructName {
        name: Identifier,
    },

    MissingStructField {
        expected: String,
    },

    MissingTupleStructElement {
        expected: String,
    },

    MissingValue {
        expected: String,
    },

    TypeMismatchArray {
        expected: String,
        found: String,
    },

    TypeMismatchBinaryExpr {
        expected: String,
        found: String,
    },

    TypeMismatchReturnType {
        expected: String,
        found: String,
    },

    TypeMismatchValues {
        expected: String,
        found: String,
    },

    TypeMismatchVariable {
        name: Identifier,
        expected: String,
        found: String,
    },

    UndefinedPath {
        name: Identifier,
    },

    UndefinedStruct {
        name: Identifier,
    },

    UndefinedVariable {
        name: Identifier,
    },

    UnexpectedAttribute {
        name: String,
        msg: String,
    },

    UnexpectedType {
        expected: String,
        found: String,
    },

    #[default]
    UnknownError,
}

impl fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticErrorKind::ConversionError { from, into } => {
                write!(f, "conversion error. Unable to convert {from} into {into}")
            }
            SemanticErrorKind::DuplicateImport { name } => {
                write!(f, "duplicate import: `{name}`")
            }
            SemanticErrorKind::DuplicateVariable { name } => {
                write!(f, "duplicate variable: `{name}`")
            }
            SemanticErrorKind::InvalidPathIdentifier { name } => {
                write!(f, "invalid path identifier: `{name}`")
            }

            SemanticErrorKind::InvalidStructName { name } => {
                write!(f, "invalid struct name: `{name}`")
            }
            SemanticErrorKind::MissingStructField { expected } => {
                write!(f, "struct field not found. Expected {expected}, found none")
            }
            SemanticErrorKind::MissingTupleStructElement { expected } => {
                write!(
                    f,
                    "tuple struct element not found. Expected {expected}, found none"
                )
            }
            SemanticErrorKind::MissingValue { expected } => {
                write!(f, "value not found. Expected {expected}, found none")
            }
            SemanticErrorKind::TypeMismatchArray { expected, found } => write!(
                f,
                "array element types do not match. Expected {expected}, found {found}"
            ),
            SemanticErrorKind::TypeMismatchBinaryExpr { expected, found } => write!(
                f,
                "type mismatch in binary expression. Expected {expected}, found {found}"
            ),
            SemanticErrorKind::TypeMismatchReturnType { expected, found } => write!(
                f,
                "value type does not match return type. Expected {expected}, found {found}"
            ),

            SemanticErrorKind::TypeMismatchValues { expected, found } => write!(
                f,
                "type mismatch between values. Expected {expected}, found {found}"
            ),

            SemanticErrorKind::TypeMismatchVariable {
                name,
                expected,
                found,
            } => {
                write!(
                    f,
                    "type mismatch for `{name}`. Expected {expected}, found {found}"
                )
            }

            SemanticErrorKind::UndefinedPath { name } => write!(f, "undefined path: `{name}`"),

            SemanticErrorKind::UndefinedStruct { name } => write!(f, "undefined struct: `{name}`"),
            SemanticErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable: `{name}`",)
            }

            SemanticErrorKind::UnexpectedAttribute { name, msg } => {
                write!(f, "unexpected attributes(s): `{name}`. {msg}")
            }

            SemanticErrorKind::UnexpectedType { expected, found } => {
                write!(f, "unexpected type(s). Expected {expected}, found {found}")
            }

            SemanticErrorKind::UnknownError => write!(f, "unknown semantic analysis error"),
        }
    }
}

impl Error for SemanticErrorKind {}
