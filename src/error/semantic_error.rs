#![allow(dead_code)]

use core::fmt;
use std::error::Error;

use crate::ast::{Identifier, UInt};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    ArgumentCountMismatch {
        name: Identifier,
        expected: usize,
        found: usize,
    },

    ConversionError {
        from: String,
        into: String,
    },

    InvalidVariableIdentifier {
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
        
    TypeMismatchLetStmt {
        value_type: String,
        declared_type: String,
    },

    TypeMismatchMatchExpr {
        loc: String,
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
        struct_name: Identifier,
        field_name: Identifier,
    },

    UndefinedFunction {
        name: Identifier,
    },

    UndefinedModule {
        name: Identifier,
    },

    UndefinedScope,

    UndefinedStruct {
        name: Identifier,
    },

    UndefinedSymbol {
        name: Identifier,
    },

   UndefinedType {
        name: Identifier,
    },

    UndefinedVariable {
        name: Identifier,
    },


    UnexpectedKeyword {
        expected: String,
        found: String,
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
            SemanticErrorKind::ArgumentCountMismatch {name,  expected, found } => {
                write!(
                    f,
                    "argument count mismatch in function `{name}()`. Expected {expected} arguments, found {found}"
                )
            }
            SemanticErrorKind::ConversionError { from, into } => {
                write!(f, "conversion error. Unable to convert {from} into {into}")
            }
       
            SemanticErrorKind::InvalidVariableIdentifier { name } => {
                write!(f, "invalid variable identifier: `{name}`")
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
                write!(f, "unable to cast {} as {}", from, to)
            },
            SemanticErrorKind::TypeMismatchArray { expected, found } => write!(
                f,
                "array element types do not match. Expected {expected}, found {found}"
            ),
            SemanticErrorKind::TypeMismatchMatchExpr { loc, expected, found } => write!(
                f,
                "{loc} types do not match in match expression. Expected {expected}, found {found}"
            ),
            SemanticErrorKind::TypeMismatchArgument { name, expected, found } => write!(
                f,
                "`{name}` type does not match defined parameter type. Expected {expected}, found {found}"
            ),
            SemanticErrorKind::TypeMismatchBinaryExpr { expected, found } => write!(
                f,
                "invalid operation: type mismatch in binary expression. Expected {expected}, found {found}"
            ),
            SemanticErrorKind::TypeMismatchLetStmt {value_type, declared_type } => write!(
                f,
                "declared type {declared_type} does not match value's type: {value_type}"
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
            SemanticErrorKind::UndefinedField { struct_name , field_name} => write!(f, "struct `{struct_name}` has no field `{field_name}`"),  
            
            SemanticErrorKind::UndefinedFunction { name } => write!(f, "no function `{name}()` in current scope"),
 
            SemanticErrorKind::UndefinedModule { name } => write!(f, "undefined module: `{name}`"),

            SemanticErrorKind::UndefinedScope => write!(f, "attempted to access undefined scope"),

            SemanticErrorKind::UndefinedStruct { name } => write!(f, "no struct `{name}` in current scope"),  

            SemanticErrorKind::UndefinedSymbol { name } => write!(f, "undefined symbol: `{name}`"),     

            SemanticErrorKind::UndefinedType { name } => write!(f, "no type `{name}` in current scope"),
            
            SemanticErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable: `{name}`",)
            }

            SemanticErrorKind::UnexpectedKeyword { expected, found } => write!(f, "unexpected keyword. Expected {expected}, found `{found}`"),

            SemanticErrorKind::UnexpectedSymbol { name, expected, found } => write!(f, "unexpected symbol for `{name}`. Expected {expected}, found `{found}`"),

            SemanticErrorKind::UnexpectedType { expected, found } => {
                write!(f, "unexpected type(s). Expected {expected}, found {found}")
            }

            SemanticErrorKind::UnknownError => write!(f, "unknown semantic analysis error"),
        }
    }
}

impl Error for SemanticErrorKind {}
