#![allow(dead_code)]

mod expression;
mod item;
mod statement;
mod types;

use std::collections::HashMap;

pub use self::{expression::*, item::*, statement::*, types::*};

///////////////////////////////////////////////////////////////////////////
/// LITERAL
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different literal AST nodes.
#[derive(Debug, Clone)]
pub enum Literal {
    Int(Int),
    UInt(UInt),
    BigUInt(BigUInt),
    Byte(Byte),
    Bytes(Bytes),
    Hash(Hash),
    Str(Str),
    Char(char),
    Bool(bool),
}

///////////////////////////////////////////////////////////////////////////
/// IDENTIFIER
///////////////////////////////////////////////////////////////////////////

/// Wrapper type, turning a `String` into an `Identifier` AST node.
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(pub String);

///////////////////////////////////////////////////////////////////////////
/// KEYWORDS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different keyword AST nodes.
#[derive(Debug, Clone)]
pub enum Keyword {
    Import,
    Mod,
    Package,
    SelfKeyword,
    SelfType,
    Super,
    Pub,
    As,
    Const,
    Static,
    Alias,
    Func,
    Struct,
    Enum,
    Trait,
    Impl,
    If,
    Else,
    Match,
    Loop,
    For,
    In,
    While,
    Break,
    Continue,
    Return,
    Unsafe,
    Let,
    Mut,
    Some,
    None,
    Ok,
    Err,
}

#[derive(Debug, Clone)]
pub enum InnerAttr {
    Contract,
    Interface,
    Library,
    Script,
}

#[derive(Debug, Clone)]
pub enum OuterAttr {
    Calldata,
    Constructor,
    Error,
    Event,
    Extern,
    Modifier,
    Payable,
    Storage,
    Test,
    Topic,
    View,
}

///////////////////////////////////////////////////////////////////////////
/// DELIMITERS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different delimiter AST nodes.
#[derive(Debug, Clone)]
pub enum Delimiter {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
}

///////////////////////////////////////////////////////////////////////////
/// PUNCTUATION
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different unary operator AST nodes.
#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,       // `-`
    Not,          // `!`
    Reference,    // `&`
    MutReference, // `&mut`
    Dereference,  // `*`
}

/// Enum representing the different binary operator AST nodes.
#[derive(Debug, Clone)]
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
    Exponentiation,
}

/// Struct representing the unwrap operator `?`.
#[derive(Debug, Clone)]
pub struct UnwrapOp(pub ());

/// Enum representing the different range operator AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub enum RangeOp {
    RangeExclusive, // `..`
    RangeInclusive, // `..=`
}

/// Enum representing the different separator (punctuation) AST nodes.
#[derive(Debug, Clone)]
pub enum Separator {
    Colon,
    Semicolon,
    Comma,
    Dot,
    DblColon,
    ColonColonAsterisk,
    ThinArrow,
    FatArrow,
    Underscore,
}

///////////////////////////////////////////////////////////////////////////
/// NODE GROUPS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different expression AST nodes.
/// `Expression` nodes always produce or evaluate to a value and may have (side) effects.
#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Path(PathExpr),
    MethodCall(MethodCallExpr),
    FieldAccess(FieldAccessExpr),
    Call(CallExpr),
    Index(IndexExpr),
    TupleIndex(TupleIndexExpr),
    Unwrap(UnwrapExpr),
    Unary(UnaryExpr),
    TypeCast(TypeCastExpr),
    Binary(BinaryExpr),
    Grouped(GroupedExpr),
    Range(RangeExpr),
    Return(ReturnExpr),
    Break(BreakExpr),
    Continue(ContinueExpr),
    Underscore(UnderscoreExpr),
    Closure(ClosureExpr),
    Array(ArrayExpr),
    Tuple(TupleExpr),
    Struct(StructExpr),
    TupleStruct(TupleStructExpr),
    Block(BlockExpr),
    If(IfExpr),       // condition, true, false
    Match(MatchExpr), // scrutinee, body
    ForIn(ForInExpr), // variable, iterable, body
    While(WhileExpr), // while, condition, body
    SomeExpr(SomeExpr),
    NoneExpr(NoneExpr),
    ResultExpr(ResultExpr),
}

#[derive(Debug, Clone)]
pub enum AssigneeExpr {
    ArrayIndex(ArrayIndex),
    CompoundAssignmentOperand(CompoundAssignmentOperand),
    DereferencedOperand(DereferencedOperand),
    FieldAccessObject(FieldAccessObject),
    FunctionCallCallee(FunctionCallCallee),
    IndexedArray(IndexedArray),
    LetStatementInitializer(LetStatementInitializer),
    MethodCallReceiver(MethodCallReceiver),
    Scrutinee(Scrutinee),
    StaticVariable(StaticVariable),
    TupleIndexOperand(TupleIndexOperand),
    Array(Vec<AssigneeExpr>),
    GroupedExpr(Box<AssigneeExpr>),
    Tuple(Vec<AssigneeExpr>),
    PathExpr(PathExpr),
    Struct(Vec<HashMap<Identifier, AssigneeExpr>>),
    TupleStruct(Vec<AssigneeExpr>),
    UnderscoreExpr(UnderscoreExpr),
}

/// Enum representing the different statement AST nodes, which are built up of expressions.
/// A `Statement` is a component of a block, which is a component of an outer expression
/// or function.
#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStmt),
    Item(Item),
    Expression(ExpressionStmt),
}

/// Enum representing the different item nodes in the AST.
/// An item is a component of a package, organized by a set of modules.
#[derive(Debug, Clone)]
pub enum Item {
    ImportDecl(ImportDecl),
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    StaticItemDecl(StaticItemDecl),
    ModuleDef(Box<ModuleDef>),
    TraitDef(TraitDef),
    EnumDef(EnumDef),
    StructDef(StructDef),
    InherentImplDef(InherentImplDef),
    TraitImplDef(TraitImplDef),
    FunctionDef(FunctionDef),
}

/// Enum representing the language's different types, which help to define a value's
/// memory interpretation and the appropriate operations that may be performed.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // primitives
    I32(String),
    I64(String),
    I128(String),
    U8(String),
    U16(String),
    U32(String),
    U64(String),
    U128(String),
    U256(String),
    U512(String),
    Byte(String), // `u8`
    B2(String),
    B4(String),
    B8(String),
    B16(String),
    B32(String),
    H160(String),
    H256(String),
    H512(String),
    Str(String), // `Vec<u8>`
    Char(String),
    Bool(String),

    UnitType, // ()

    // built-in collections
    Array {
        element_type: Box<Type>,
        num_elements: UInt,
    },
    Tuple(Vec<Type>),

    UserDefined(String), // struct, enum, trait, alias, constant (paths / items)

    Function {
        function_name: Identifier,
        return_type_opt: Option<Box<Type>>,
    },
    Reference(Box<Type>), //  `&Type` / `&mut Type`
    SelfType(String),

    Vec(Box<Type>),
    Mapping {
        key_type: Box<Type>,
        value_type: Box<Type>,
    },

    Option(Box<Type>),
    Result {
        ok: Box<Type>,
        err: Box<Type>,
    },
}

///////////////////////////////////////////////////////////////////////////
/// HELPER FUNCTIONS
///////////////////////////////////////////////////////////////////////////

/// Helper function to turn a slice into a `Bytes`.
pub fn get_bytes(value: &[u8]) -> Bytes {
    let bytes = match value.len() {
        0 => panic!("empty slice"),
        1 => panic!("byte string literals must have more than one character"),
        2 => Bytes::B2(B2::from_slice(value)),
        3 => Bytes::B4(B4::from(&pad_zeroes::<3, 4>(value))),
        4 => Bytes::B4(B4::from_slice(value)),
        5 => Bytes::B8(B8::from(&pad_zeroes::<5, 8>(value))),
        6 => Bytes::B8(B8::from(&pad_zeroes::<6, 8>(value))),
        7 => Bytes::B8(B8::from(&pad_zeroes::<7, 8>(value))),
        8 => Bytes::B8(B8::from_slice(value)),
        9 => Bytes::B16(B16::from(&pad_zeroes::<9, 16>(value))),
        10 => Bytes::B16(B16::from(&pad_zeroes::<10, 16>(value))),
        11 => Bytes::B16(B16::from(&pad_zeroes::<11, 16>(value))),
        12 => Bytes::B16(B16::from(&pad_zeroes::<12, 16>(value))),
        13 => Bytes::B16(B16::from(&pad_zeroes::<13, 16>(value))),
        14 => Bytes::B16(B16::from(&pad_zeroes::<14, 16>(value))),
        15 => Bytes::B16(B16::from(&pad_zeroes::<15, 16>(value))),
        16 => Bytes::B16(B16::from_slice(value)),
        17 => Bytes::B32(B32::from(&pad_zeroes::<17, 32>(value))),
        18 => Bytes::B32(B32::from(&pad_zeroes::<18, 32>(value))),
        19 => Bytes::B32(B32::from(&pad_zeroes::<19, 32>(value))),
        20 => Bytes::B32(B32::from(&pad_zeroes::<20, 32>(value))),
        21 => Bytes::B32(B32::from(&pad_zeroes::<21, 32>(value))),
        22 => Bytes::B32(B32::from(&pad_zeroes::<22, 32>(value))),
        23 => Bytes::B32(B32::from(&pad_zeroes::<23, 32>(value))),
        24 => Bytes::B32(B32::from(&pad_zeroes::<24, 32>(value))),
        25 => Bytes::B32(B32::from(&pad_zeroes::<25, 32>(value))),
        26 => Bytes::B32(B32::from(&pad_zeroes::<26, 32>(value))),
        27 => Bytes::B32(B32::from(&pad_zeroes::<27, 32>(value))),
        28 => Bytes::B32(B32::from(&pad_zeroes::<28, 32>(value))),
        29 => Bytes::B32(B32::from(&pad_zeroes::<29, 32>(value))),
        30 => Bytes::B32(B32::from(&pad_zeroes::<30, 32>(value))),
        31 => Bytes::B32(B32::from(&pad_zeroes::<31, 32>(value))),
        32 => Bytes::B32(B32::from_slice(value)),
        _ => panic!("slice too big"),
    };

    bytes
}

/// Pad an input byte slice with zeroes to turn it into a fixed size array.
/// Useful when converting a byte string literal into a `Bytes` literal.
#[track_caller]
fn pad_zeroes<const A: usize, const B: usize>(slice: &[u8]) -> [u8; B] {
    assert!(B >= A, "input size is greater than target size");

    let arr: [u8; A] = slice
        .try_into()
        .expect("unable to convert slice into fixed size byte array");
    let mut target = [0; B];

    target[..A].copy_from_slice(&arr);
    target
}
