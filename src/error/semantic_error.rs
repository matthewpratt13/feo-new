use core::fmt;
use std::error::Error;

use crate::ast::{Identifier, Keyword, Type, UInt};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    ConstantReassignment {
        name: Identifier,
    },

    FuncArgCountMismatch {
        function_path: Identifier,
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

    ModuleErrors {
        name: Identifier
    },

    StructArgCountMismatch {
        struct_path: Identifier,
        expected: usize,
        found: usize,
    },

    TupleIndexOutOfBounds {
        len: UInt,
        i: UInt
    },

    TypeBoundNotSatisfied {
        generic_name: Identifier,
        expected_bound: Identifier,
        found_type: Type,
    },

    TypeCastError {
        from: Type,
        to: Type,
    },

    TypeMismatchArgument {
        arg_id: Identifier,
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

    TypeMismatchUnification {
        expected: Identifier,
        found: Identifier,
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
    
    TypeMismatchResultExpr {
        variant: Keyword,
        expected: Type,
        found: Type,
    },

    TypeMismatchTypeBound {
        expected: Identifier,
        found: Identifier,
    },

    TypeMismatchValues {
        expected: Type,
        found: Type,
    },

    TypeMismatchVariable {
        var_id: Identifier,
        expected: String,
        found: Type,
    },

    UndeclaredGenericParams {
        found: String,
    },

    UndefinedField {
        struct_path: Identifier,
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
    
    UndefinedVariable {
        name: Identifier,
    },

    UnexpectedKeyword {
        expected: String,
        found: Keyword,
    },

    UnexpectedPath {
        expected: String,
        found: Identifier,
    }, 

    UnexpectedStructField {
        field_name: Identifier,
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

    #[default]
    UnknownError,
}

impl fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticErrorKind::ConstantReassignment{ name } => {
                write!(f, "cannot reassign constant: `{name}`")
            }  
            SemanticErrorKind::FuncArgCountMismatch {function_path,  expected, found } => {
                write!(
                    f,
                    "argument count mismatch in function `{function_path}()`. Expected {expected} arguments, found {found}"
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
            SemanticErrorKind::ModuleErrors { name } => {
                write!(f, "detected errors in module `{name}`")
            }
            SemanticErrorKind::StructArgCountMismatch { struct_path, expected, found } => {
                write!(f, "argument count mismatch in struct `{struct_path}`. Expected {expected} arguments, found {found}")
            }
            SemanticErrorKind::TupleIndexOutOfBounds { len, i } => {
                write!(f, "tuple index out of bounds. Index is {i}, length is {len}")
            }
            SemanticErrorKind::TypeBoundNotSatisfied { generic_name, expected_bound: bound, found_type: found } => write!(f, "type {found} has generic parameter `{generic_name}` that does not satisfy type bound `{bound}`"),
            SemanticErrorKind::TypeCastError { from, to } => {
                write!(f, "unable to cast `{}` as `{}`", from, to)
            },
            SemanticErrorKind::TypeMismatchArray { expected, found } => write!(
                f,
                "array element types do not match. Expected {expected}, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchUnification { expected, found } => write!(
                f,
                "types do not match when attempting to unify. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchMatchExpr { loc, expected, found } => write!(
                f,
                "{loc} types do not match in match expression. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchOrPatt { expected, found } => write!(
                f,
                "pattern types do not match. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchArgument { arg_id: name, expected, found } => write!(
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
                "value type does not match return type. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchResultExpr { variant, expected, found } => write!(
                f,
                "type does not match expected `{variant}` type. Expected `{expected}`, found `{found}"
            ),
            SemanticErrorKind::TypeMismatchTypeBound {  expected, found } => write!(
                f,
                "generic type bounds do not match. Expected `{expected}`, found `{found}"
            ),
            SemanticErrorKind::TypeMismatchValues { expected, found } => write!(
                f,
                "type mismatch between values. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchVariable {
                var_id: name,
                expected,
                found,
            } => {
                write!(
                    f,
                    "type mismatch for `{name}`. Expected {expected}, found `{found}`"
                )
            }
            SemanticErrorKind::UndeclaredGenericParams { found } => {
                write!(f, "undeclared generic parameter (`{found}`) in function signature. Generic parameters must be declared after `func` keyword (e.g., `func<T>`) in function definition context; or after `impl` keyword in implementation definition context (e.g., `impl<T>`); or after `trait` keyword in trait definition context (e.g., `trait<T>`)")
            }
            SemanticErrorKind::UndefinedField { struct_path: struct_name , field_name} => write!(f, "struct `{struct_name}` has no field `{field_name}`"),  
            
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

            SemanticErrorKind::UnexpectedPath { expected, found } => write!(f, "unexpected path. Expected {expected}, found `{found}`"),

            SemanticErrorKind::UnexpectedStructField { field_name: name, found } => write!(f, "unexpected field in struct `{name}`: `{found}`"),
            
            SemanticErrorKind::UnexpectedSymbol { name, expected, found } => write!(f, "unexpected symbol for `{name}`. Expected {expected}, found {found}"),
            
            SemanticErrorKind::UnexpectedType { expected, found } => {
                write!(f, "unexpected type(s). Expected {expected}, found `{found}`")
            }

            SemanticErrorKind::UnknownError => write!(f, "unknown semantic analysis error"),
        }
    }
}


impl Error for SemanticErrorKind {}
