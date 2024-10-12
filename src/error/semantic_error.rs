use core::fmt;
use std::error::Error;

use crate::ast::{Identifier, Keyword, ReferenceOp, Type};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum SemanticErrorKind {
    ArrayLengthMismatch {
        expected: usize,
        found: usize,
    },

    ConstantReassignment {
        constant_name: Identifier,
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
    
    MissingItem {
        expected: String,
    },
    
    MissingReturnType {
        expected: Type,
    },
    
    MissingStructField {
        expected: String,
    },

    MissingTraitFunctionImpl {
        func_name: Identifier,
    },

    ModuleErrors {
        name: Identifier,
    },

    ParamCountMismatch {
        expected: usize,
        found: usize,
    },

    RefOperatorMismatch {
        expected: ReferenceOp,
        found: ReferenceOp,
    },

    StructArgCountMismatch {
        struct_path: Identifier,
        expected: usize,
        found: usize,
    },

    TupleIndexOutOfBounds {
        len: usize,
        i: usize,
    },

    TupleLengthMismatch {
        expected: usize,
        found: usize,
    },

    TypeBoundCountMismatch {
        expected: usize,
        found: usize,
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

    TypeMismatchArg {
        arg_id: Identifier,
        expected: Type,
        found: Type,
    },

    TypeMismatchArrayElems {
        expected: String,
        found: Type,
    },

    TypeMismatchNumeric {
        expected: String,
        found: Type,
    },

    TypeMismatchDeclaredType {
        actual_type: Type,
        declared_type: Type,
    },

    TypeMismatchInnerType {
        context: String,
        expected: String,
        found: Type,
    },

    TypeMismatchMappingKey {
        expected: Type,
        found: Type,
    },

    TypeMismatchMappingValue {
        expected: Type,
        found: Type,
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

    TypeMismatchParam {
        expected: Type,
        found: Type,
    },

    TypeMismatchResultExpr {
        variant: Keyword,
        expected: Type,
        found: Type,
    },

    TypeMismatchReturnType {
        expected: Type,
        found: Type,
    },

    TypeMismatchSelfParam {
        expected: String,
        found: String,
    },

    TypeMismatchTupleElems {
        expected: Type,
        found: Type,
    },

    TypeMismatchTypeBound {
        expected: Identifier,
        found: Identifier,
    },

    TypeMismatchUnification {
        expected: Identifier,
        found: Identifier,
    },

    TypeMismatchUserDefined {
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

    UndefinedFunc {
        name: Identifier,
    },

    UndefinedLibrary {
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

    UnexpectedInferredType,

    UnexpectedKeyword {
        expected: String,
        found: Keyword,
    },

    UnexpectedParam {
        expected: String,
        found: Identifier,
    },

    // UnexpectedPath {
    //     expected: String,
    //     found: Identifier,
    // },
    UnexpectedReturnType {
        found: Type,
    },

    UnexpectedStructField {
        struct_name: Identifier,
        found: Identifier,
    },

    UnexpectedSymbol {
        name: Identifier,
        expected: String,
        found: String,
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
            SemanticErrorKind::ArrayLengthMismatch { expected, found } => {
                write!(f, "array length mismatch. Expected {expected} elements, found {found}")
            }
            SemanticErrorKind::ConstantReassignment { constant_name: name } => {
                write!(f, "cannot reassign constant `{name}`")
            }
            SemanticErrorKind::FuncArgCountMismatch { function_path,  expected, found } => {
                write!(
                    f,
                    "unexpected number of arguments given for function `{function_path}()`. Expected {expected} arguments, found {found}"
                )
            }
            SemanticErrorKind::InvalidVariableIdentifier { name } => {
                write!(f, "invalid variable identifier: `{name}`")
            }
            SemanticErrorKind::MethodParamCountError => {
                write!(f, "too many `self` parameters")
            }
            SemanticErrorKind::MissingItem { expected } => {
                write!(f, "item not found. Expected {expected}, found none")
            }
            SemanticErrorKind::MissingReturnType { expected } => write!(f, "return type not found. Expected `{expected}`, found none"),

            SemanticErrorKind::MissingStructField { expected } => {
                write!(f, "struct field not found. Expected {expected}, found none")
            }
            SemanticErrorKind::MissingTraitFunctionImpl { func_name } => {
                write!(f, "implementation not found for trait function: `{func_name}`")
            }
            SemanticErrorKind::ModuleErrors { name } => {
                write!(f, "detected errors in module `{name}`")
            }
            SemanticErrorKind::ParamCountMismatch {expected, found } => {
                write!(
                    f,
                    "parameter count mismatch. Expected {expected} parameters, found {found}"
                )
            }
            SemanticErrorKind::RefOperatorMismatch { expected, found } => write!(f, "reference operator mismatch. Expected {expected}, found {found}"),

            SemanticErrorKind::StructArgCountMismatch { struct_path, expected, found } => {
                write!(f, "argument count mismatch in struct `{struct_path}`. Expected {expected} arguments, found {found}")
            }
            SemanticErrorKind::TupleIndexOutOfBounds { len, i } => {
                write!(f, "tuple index out of bounds. Index is {i}, length is {len}")
            },

            SemanticErrorKind::TupleLengthMismatch { expected, found } => {
                write!(f, "tuple length mismatch. Expected {expected} elements, found {found}")
            },

            SemanticErrorKind::TypeBoundCountMismatch { expected, found } => write!(f, "unexpected number of type bounds. Expected {expected}, found {found}"),

            SemanticErrorKind::TypeBoundNotSatisfied { generic_name, expected_bound: bound, found_type: found } => write!(f, "type {found} has generic parameter `{generic_name}` that does not satisfy type bound `{bound}`"),

            SemanticErrorKind::TypeCastError { from, to } => {
                write!(f, "unable to cast `{}` as `{}`", from, to)
            },
            SemanticErrorKind::TypeMismatchArg { arg_id: name, expected, found } => write!(
                f,
                "argument `{name}` type does not match defined parameter type. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchArrayElems { expected, found } => write!(
                f,
                "array element types do not match. Expected {expected}, found `{found}`"
            ),
          
            SemanticErrorKind::TypeMismatchDeclaredType {actual_type, declared_type } => write!(
                f,
                "declared type `{declared_type}` does not match value's type: `{actual_type}`"
            ),
            SemanticErrorKind::TypeMismatchInnerType { context, expected, found } => write!(f, "inner type mismatch for {context} type. Expected {expected}, found {found}"),

            SemanticErrorKind::TypeMismatchMappingKey { expected, found } => write!(f, "unexpected mapping key type. Expected {expected}, found {found}"),

            SemanticErrorKind::TypeMismatchMappingValue { expected, found } => write!(f, "unexpected mapping value type. Expected {expected}, found {found}"),

            SemanticErrorKind::TypeMismatchMatchExpr { loc, expected, found } => write!(
                f,
                "{loc} types do not match in match expression. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchNumeric { expected, found } => write!(
                f,
                "invalid operation: numeric types do not match. Expected {expected}, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchOrPatt { expected, found } => write!(
                f,
                "pattern types do not match. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchParam { expected, found } => write!(f, "function parameters do not match. Expected `{expected}`, found `{found}`"),

            SemanticErrorKind::TypeMismatchResultExpr { variant, expected, found } => write!(
                f,
                "type does not match expected `{variant}` type. Expected `{expected}`, found `{found}"
            ),
            SemanticErrorKind::TypeMismatchReturnType { expected, found } => write!(
                f,
                "value type does not match return type. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchSelfParam { expected, found } => {
                write!(f, "type mismatch between `self` parameters. Expected {expected}, found {found}")
            },
            SemanticErrorKind::TypeMismatchTupleElems { expected, found } => write!(
                f,
                "tuple element types do not match. Expected `{expected}`, found `{found}`"
            ),
            SemanticErrorKind::TypeMismatchTypeBound {  expected, found } => write!(
                f,
                "generic type bounds do not match. Expected `{expected}`, found `{found}"
            ),
            SemanticErrorKind::TypeMismatchUnification { expected, found } => write!(
                f,
                "types do not match when attempting to unify. Expected `{expected}`, found `{found}`"
            ),

            SemanticErrorKind::TypeMismatchUserDefined { expected, found } => write!(f, "user-defined types do not match. Expected `{expected}`, found `{found}`"),

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

            SemanticErrorKind::UndefinedFunc { name } => write!(f, "no function `{name}()` in current scope"),

            SemanticErrorKind::UndefinedLibrary { name } => write!(f, "undefined library: `{name}`"),

            SemanticErrorKind::UndefinedScope => write!(f, "attempted to access undefined scope"),

            SemanticErrorKind::UndefinedStruct { name } => write!(f, "no struct `{name}` in current scope"),

            SemanticErrorKind::UndefinedSymbol { name } => write!(f, "undefined symbol: {name}"),

            SemanticErrorKind::UndefinedType { name } => write!(f, "no type `{name}` in current scope"),

            SemanticErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable: `{name}`")
            }

            SemanticErrorKind::UnexpectedInferredType => write!(f, "unexpected inferred type. Expected concrete or generic type"),

            SemanticErrorKind::UnexpectedKeyword { expected, found } => write!(f, "unexpected keyword. Expected {expected}, found `{found}`"),

            SemanticErrorKind::UnexpectedParam { expected, found } => write!(f, "unexpected parameter. Expected {expected}, found `{found}`"),

            // SemanticErrorKind::UnexpectedPath { expected, found } => write!(f, "unexpected path. Expected {expected}, found `{found}`"),

            SemanticErrorKind::UnexpectedReturnType { found } => write!(f, "unexpected return type. Did not expected a return type, but found `{found}`"),

            SemanticErrorKind::UnexpectedStructField { struct_name: name, found } => write!(f, "unexpected field in struct `{name}`: `{found}`"),

            SemanticErrorKind::UnexpectedSymbol { name, expected, found } => write!(f, "unexpected symbol for `{name}`. Expected {expected}, found {found}"),

            SemanticErrorKind::UnexpectedType { expected, found } => {
                write!(f, "unexpected type(s). Expected {expected}, found `{found}`")
            }

            SemanticErrorKind::UnknownError => write!(f, "unknown semantic analysis error"),
        }
    }
}

impl Error for SemanticErrorKind {}
