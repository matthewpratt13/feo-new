use core::fmt;
use std::error::Error;

use crate::ast::{Identifier, Keyword, Type, UInt};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    ConstantReassignment {
        name: Identifier,
    },

    FuncArgCountMismatch {
        name: Identifier,
        expected: usize,
        found: usize,
    },

    InvalidVariableIdentifier {
        name: Identifier,
    },
    
    MethodParamCountError,

    MissingStructField {
        expected: String,
    },

    MissingValue {
        expected: String,
    },

    StructArgCountMismatch {
        name: Identifier,
        expected: usize,
        found: usize,
    },

    TupleIndexOutOfBounds {
        len: UInt,
        i: UInt
    },

    TypeCastError {
        from: Type,
        to: Type,
    },

    TypeMismatchArgument {
        name: Identifier,
        expected: Type,
        found: Type,
    },

    TypeMismatchArray {
        expected: String,
        found: Type,
    },
    
    TypeMismatchBinaryExpr {
        expected: String,
        found: Type,
    },
        
    TypeMismatchDeclaredType {
        actual_type: Type,
        declared_type: Type,
    },

    TypeMismatchMatchExpr {
        loc: String,
        expected: Type,
        found: Type,
    },

    TypeMismatchOrPatt {
        expected: Type,
        found: Type,
    },

    TypeMismatchReturnType {
        expected: Type,
        found: Type,
    },
    
    TypeMismatchValues {
        expected: Type,
        found: Type,
    },

    TypeMismatchVariable {
        name: Identifier,
        expected: String,
        found: Type,
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
        name: String,
    },

    UndefinedType {
        name: Identifier,
    },

    UnexpectedKeyword {
        expected: String,
        found: Keyword,
    }, 

    UnexpectedStructField {
        name: Identifier,
        found: Identifier,
    },
    
    UnexpectedSymbol {
        name: Identifier,
        expected: String,
        found: String
    },
    
    UnexpectedType {
        expected: String,
        found: Type,
    },
    
    UndefinedVariable {
        name: Identifier,
    },

    #[default]
    UnknownError,
}

impl fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticErrorKind::ConstantReassignment{ name } => {
                write!(f, "cannot reassign constant: `{name}`")
            }  
            SemanticErrorKind::FuncArgCountMismatch {name,  expected, found } => {
                write!(
                    f,
                    "argument count mismatch in function `{name}()`. Expected {expected} arguments, found {found}"
                )
            }
            SemanticErrorKind::InvalidVariableIdentifier { name } => {
                write!(f, "invalid variable identifier: `{name}`")
            }
            SemanticErrorKind::MethodParamCountError => {
                write!(f, "too many `self` parameters")
            }
            SemanticErrorKind::MissingStructField { expected } => {
                write!(f, "struct field not found. Expected {expected}, found none")
            }
            SemanticErrorKind::MissingValue { expected } => {
                write!(f, "value not found. Expected {expected}, found none")
            }
            SemanticErrorKind::StructArgCountMismatch { name, expected, found } => {
                write!(f, "argument count mismatch in struct `{name}`. Expected {expected} arguments, found {found}")
            }
            SemanticErrorKind::TupleIndexOutOfBounds { len, i } => {
                write!(f, "tuple index out of bounds. Index is {i}, length is {len}")
            }
            SemanticErrorKind::TypeCastError { from, to } => {
                write!(f, "unable to cast `{}` as `{}`", from, to)
            },
            SemanticErrorKind::TypeMismatchArray { expected, found } => write!(
                f,
                "array element types do not match. Expected {expected}, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchMatchExpr { loc, expected, found } => write!(
                f,
                "{loc} types do not match in match expression. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchOrPatt { expected, found } => write!(
                f,
                "pattern types do not match. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchArgument { name, expected, found } => write!(
                f,
                "`{name}` type does not match defined parameter type. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchBinaryExpr { expected, found } => write!(
                f,
                "invalid operation: type mismatch in binary expression. Expected {expected}, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchDeclaredType {actual_type, declared_type } => write!(
                f,
                "declared type `{declared_type}` does not match value's type: `{actual_type}`"
            ),
            SemanticErrorKind::TypeMismatchReturnType { expected, found } => write!(
                f,
                "value type does not match return type. Expected `{expected}`, found `{found}"
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
                    "type mismatch for `{name}`. Expected {expected}, found `{found}`"
                )
            }
            SemanticErrorKind::UndefinedField { struct_name , field_name} => write!(f, "struct `{struct_name}` has no field `{field_name}`"),  
            
            SemanticErrorKind::UndefinedFunction { name } => write!(f, "no function `{name}()` in current scope"),
 
            SemanticErrorKind::UndefinedModule { name } => write!(f, "undefined module: `{name}`"),

            SemanticErrorKind::UndefinedScope => write!(f, "attempted to access undefined scope"),

            SemanticErrorKind::UndefinedStruct { name } => write!(f, "no struct `{name}` in current scope"),  

            SemanticErrorKind::UndefinedSymbol { name } => write!(f, "undefined symbol: {name}"),     

            SemanticErrorKind::UndefinedType { name } => write!(f, "no type `{name}` in current scope"),
            
            SemanticErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable: `{name}`")
            }
          
            SemanticErrorKind::UnexpectedKeyword { expected, found } => write!(f, "unexpected keyword. Expected {expected}, found `{found}`"),

            SemanticErrorKind::UnexpectedStructField { name, found } => write!(f, "unexpected field in struct `{name}`: `{found}`"),
            
            SemanticErrorKind::UnexpectedSymbol { name, expected, found } => write!(f, "unexpected symbol for `{name}`. Expected {expected}, found {found}"),
            
            SemanticErrorKind::UnexpectedType { expected, found } => {
                write!(f, "unexpected type(s). Expected {expected}, found `{found}`")
            }

            SemanticErrorKind::UnknownError => write!(f, "unknown semantic analysis error"),
        }
    }
}

impl Error for SemanticErrorKind {}
