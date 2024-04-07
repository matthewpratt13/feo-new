#![allow(dead_code)]

use crate::{
    parser::StructField, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B2, B20, B21, B22, B23,
    B24, B25, B26, B27, B28, B29, B3, B30, B31, B32, B4, B5, B6, B7, B8, B9, H160, H256, U256,
};

/// Enum representing the different signed integer types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntKind {
    I32(i32),
    I64(i64),
    I128(i128),
}

/// Enum representing the different unsigned integer types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UIntKind {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

/// Enum representing the different byte array types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bytes {
    B2(B2),
    B3(B3),
    B4(B4),
    B5(B5),
    B6(B6),
    B7(B7),
    B8(B8),
    B9(B9),
    B10(B10),
    B11(B11),
    B12(B12),
    B13(B13),
    B14(B14),
    B15(B15),
    B16(B16),
    B17(B17),
    B18(B18),
    B19(B19),
    B20(B20),
    B21(B21),
    B22(B22),
    B23(B23),
    B24(B24),
    B25(B25),
    B26(B26),
    B27(B27),
    B28(B28),
    B29(B29),
    B30(B30),
    B31(B31),
    B32(B32),
}

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
    H160(H160),
    H256(H256),
    Byte(u8),
    Bytes(Bytes),
    String(Vec<u8>),
    Char(char),
    Bool(bool),
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
    H160,
    H256,
    Byte,
    Bytes,
    String,
    Char,
    Bool,

    // built-in collections
    Array,
    Tuple,

    UserDefined, // struct, enum, trait, alias, constant (path types / items)

    Function,
    Reference, // e.g., `&Type` / `&mut Type`
    SelfType,
}

pub fn get_bytes(value: &[u8]) -> Bytes {
    let bytes = match value.len() {
        0 => panic!("empty slice"),
        1 => panic!("byte arrays must have more than one element"),
        2 => Bytes::B2(B2::from_slice(value)),
        3 => Bytes::B3(B3::from_slice(value)),
        4 => Bytes::B4(B4::from_slice(value)),
        5 => Bytes::B5(B5::from_slice(value)),
        6 => Bytes::B6(B6::from_slice(value)),
        7 => Bytes::B7(B7::from_slice(value)),
        8 => Bytes::B8(B8::from_slice(value)),
        9 => Bytes::B9(B9::from_slice(value)),
        10 => Bytes::B10(B10::from_slice(value)),
        11 => Bytes::B11(B11::from_slice(value)),
        12 => Bytes::B12(B12::from_slice(value)),
        13 => Bytes::B13(B13::from_slice(value)),
        14 => Bytes::B14(B14::from_slice(value)),
        15 => Bytes::B15(B15::from_slice(value)),
        16 => Bytes::B16(B16::from_slice(value)),
        17 => Bytes::B17(B17::from_slice(value)),
        18 => Bytes::B18(B18::from_slice(value)),
        19 => Bytes::B19(B19::from_slice(value)),
        20 => Bytes::B20(B20::from_slice(value)),
        21 => Bytes::B21(B21::from_slice(value)),
        22 => Bytes::B22(B22::from_slice(value)),
        23 => Bytes::B23(B23::from_slice(value)),
        24 => Bytes::B24(B24::from_slice(value)),
        25 => Bytes::B25(B25::from_slice(value)),
        26 => Bytes::B26(B26::from_slice(value)),
        27 => Bytes::B27(B27::from_slice(value)),
        28 => Bytes::B28(B28::from_slice(value)),
        29 => Bytes::B29(B29::from_slice(value)),
        30 => Bytes::B30(B30::from_slice(value)),
        31 => Bytes::B31(B31::from_slice(value)),
        32 => Bytes::B32(B32::from_slice(value)),
        _ => panic!("slice too big"),
    };

    bytes
}
