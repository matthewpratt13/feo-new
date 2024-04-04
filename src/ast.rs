#![allow(dead_code)]

use crate::{
    number::{IntKind, UIntKind},
    parser::StructField,
    U256,
};

/// Enum representing the different unary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,      // `-`
    Not,         // `!`
    Reference,   // `&`
    Dereference, // `*`
}

/// Enum representing the different binary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModulusAssign,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

/// Enum representing the different literal types available in parsing.
#[derive(Debug, Clone)]
pub enum Literal {
    Int(IntKind),
    UInt(UIntKind),
    U256(U256),
    String(String),
    Char(char),
    Bool(bool),
}

/// Enum representing the different precedence levels of operators, respectively.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Unwrap,             // `?`
    Assignment,         // `=`
    CompoundAssignment, // `+=`, `-=`, `*/`, `/=`, `%=`
    LogicalOr,          // `||`
    LogicalAnd,         // `&&`
    Equal,              // `==`
    NotEqual,           // `!=`
    LessThan,           // `<`
    LessThanOrEqual,    // `<=`
    GreaterThan,        // `>`
    GreaterThanOrEqual, // `>=`
    Shift,              // `«`, `»`
    BitwiseAnd,         // `&`
    BitwiseXor,         // `^`
    BitwiseOr,          // `|`
    Sum,                // `+`
    Difference,         // `-`
    Product,            // `*`
    Quotient,           // `/`
    Remainder,          // `%`
    Unary,              // `-`, `!`, `&`, `*`
    Exponentiation,     // `**`
    Cast,               // "as"
}

/// Enum representing the different types of expressions in the AST.
/// `Expression` nodes always produce or evaluate to a value and may have (side) effects.
#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>), // condition, true, false
    ForIn(Box<Expression>, Box<Expression>, Box<Expression>),      // variable, iterable, body
    Array(Vec<Expression>),
    Tuple(Vec<Expression>),
    Block(Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    FieldAccess(Box<Expression>, String),
    Cast(Box<Expression>, Type),
    Unwrap(Box<Expression>, Box<Expression>),
    Struct(Vec<StructField>),
    Grouped(Box<Expression>),

    // TODO: parse:
    InfiniteLoop(Box<Expression>),
    WhileLoop(Box<Expression>, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    TupleStruct(Vec<Expression>),
    MethodCall(Box<Expression>, Box<Expression>),
    ClosureWithBlock(Box<Expression>, Box<Expression>),
    ClosureWithoutBlock(Box<Expression>),
    Range(Box<Expression>, Option<Box<Expression>>), // from-to, from, to, inclusive, to inclusive
    Path(Box<Expression>, Vec<Expression>),
    TupleIndex(Box<Expression>, Box<Expression>),
    Match(Box<Expression>, Vec<Expression>),
    Return(Box<Expression>),
    BreakExpression(Box<Expression>),
    ContinueExpression(Box<Expression>),
    Underscore(Box<Expression>),
}

/// Enum representing the different types of statements, which are built up of expressions.
/// A `Statement` is a component of a block, which is a component of an outer expression
/// or function.
#[derive(Debug, Clone)]
pub enum Statement {
    Let(String, Expression),
    Expr(Expression),
    Item(Item),
}

// TODO: parse:
/// Enum representing the different items in the AST.
/// An item is a component of a package, organized by a set of modules.
#[derive(Debug, Clone)]
pub enum Item {
    // definition blocks
    Function,
    FunctionSig, // (without block)
    Struct,
    TupleStruct, // (without block)
    Enum,
    Trait,
    ImplBlock,
    Module,
    ModuleSig, // (without block)

    // declarations
    TypeAlias,
    ConstantVar,
    StaticVar,
    Import,
}

// TODO: parse:
/// Enum representing the language's different types, which help to define a value's
/// memory interpretation and the appropriate operations that may be performed.
#[derive(Debug, Clone)]
pub enum Type {
    // primitives
    Int,
    UInt,
    U256,
    String,
    Char,
    Bool,

    // built-in collections
    Array,
    Tuple,

    UserDefined, // struct, enum, trait, alias, constant (path types / items)

    Function,
    Closure,

    Reference, // e.g., `&Type` / `&mut Type`
    SelfType,
}
