#![allow(dead_code)]

use core::fmt;
use std::error::Error;

use crate::ast::{Identifier, UInt};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
    },

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

    TupleIndexOutOfBounds {
        len: UInt,
        i: UInt
    },

    TypeCastError {
        from: String,
        to: String,
    },

    TypeMismatchArgument {
        name: Identifier,
        expected: String,
        found: String,
    },

    TypeMismatchArray {
        expected: String,
        found: String,
    },
    
    TypeMismatchBinaryExpr {
        expected: String,
        found: String,
        },
        
    TypeMismatchMatchArmExpression {
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

    UndefinedField {
        name: Identifier,
    },

    UndefinedFunction {
        name: Identifier,
    },

    UndefinedModule {
        name: Identifier,
    },

    UndefinedPath {
        name: Identifier,
    },

    UndefinedScope,

    UndefinedStruct {
        name: Identifier,
    },

   UndefinedType {
        name: Identifier,
    },

    UndefinedVariable {
        name: Identifier,
    },

    UnexpectedSymbol {
        name: Identifier,
        expected: String,
        found: String
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
            SemanticErrorKind::ArgumentCountMismatch { expected, found } => {
                write!(
                    f,
                    "argument count mismatch. Expected {expected} arguments, found {found}"
                )
            }
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
            SemanticErrorKind::TupleIndexOutOfBounds { len, i } => {
                write!(f, "tuple index out of bounds. Index is {i}, length is {len}")
            }
            SemanticErrorKind::TypeCastError { from, to } => {
                write!(f, "unable to cast `{}` as `{}`", from, to)
            },
            SemanticErrorKind::TypeMismatchArray { expected, found } => write!(
                f,
                "array element types do not match. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchMatchArmExpression { expected, found } => write!(
                f,
                "match arm expression types do not match. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchArgument { name, expected, found } => write!(
                f,
                "`{name}` type does not match function definition parameter type. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchBinaryExpr { expected, found } => write!(
                f,
                "type mismatch in binary expression. Expected {expected}, found {found}"
            ),
            SemanticErrorKind::TypeMismatchReturnType { expected, found } => write!(
                f,
                "value type does not match return type. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchValues { expected, found } => write!(
                f,
                "type mismatch between values. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchVariable {
                name,
                expected,
                found,
            } => {
                write!(
                    f,
                    "type mismatch for `{name}`. Expected `{expected}`, found `{found}`"
                )
            }
            SemanticErrorKind::UndefinedField { name } => write!(f, "undefined field: `{name}`"),  
            
            SemanticErrorKind::UndefinedFunction { name } => write!(f, "undefined function: `{name}`"),
 
            SemanticErrorKind::UndefinedModule { name } => write!(f, "undefined module: `{name}`"),

            SemanticErrorKind::UndefinedPath { name } => write!(f, "undefined path: `{name}`"),

            SemanticErrorKind::UndefinedScope => write!(f, "attempted to access undefined scope"),

            SemanticErrorKind::UndefinedStruct { name } => write!(f, "undefined struct: `{name}`"),     

            SemanticErrorKind::UndefinedType { name } => write!(f, "undefined type: `{name}`"),
            
            SemanticErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable: `{name}`",)
            }

            SemanticErrorKind::UnexpectedSymbol { name, expected, found } => write!(f, "unexpected symbol for `{name}`. Expected {expected}, found `{found}`"),

            SemanticErrorKind::UnexpectedType { expected, found } => {
                write!(f, "unexpected type(s). Expected {expected}, found `{found}`")
            }

            SemanticErrorKind::UnknownError => write!(f, "unknown semantic analysis error"),
        }
    }
}

impl Error for SemanticErrorKind {}
