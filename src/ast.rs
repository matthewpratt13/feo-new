#![allow(dead_code)]

mod expression;
mod item;
mod statement;
mod types;

pub use self::{expression::*, item::*, statement::*, types::*};

///////////////////////////////////////////////////////////////////////////
/// LITERAL
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different literal AST nodes.
#[derive(Debug, Clone)]
pub enum Literal {
    Int(IntKind),
    UInt(UIntKind),
    BigUInt(BigUIntKind),
    Byte(u8),
    Bytes(Bytes),
    Hash(HashKind),
    String(Vec<u8>),
    Char(char),
    Bool(bool),
}

///////////////////////////////////////////////////////////////////////////
/// IDENTIFIER
///////////////////////////////////////////////////////////////////////////

/// Wrapper type, turning a `String` into an `Identifier` AST node.
#[derive(Debug, Clone)]
pub struct Identifier(pub String);

///////////////////////////////////////////////////////////////////////////
/// KEYWORDS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different keyword AST nodes.
#[derive(Debug, Clone, PartialEq)]
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
}

///////////////////////////////////////////////////////////////////////////
/// DELIMITERS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different delimiter AST nodes.
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,       // `-`
    Not,          // `!`
    Reference,    // `&`
    MutReference, // `&mut`
    Dereference,  // `*`
}

/// Enum representing the different binary operator AST nodes.
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
#[derive(Debug, Clone, PartialEq)]
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
    Match(MatchExpr), // scrutinee, body
}

/// Enum representing the different statement AST nodes, which are built up of expressions.
/// A `Statement` is a component of a block, which is a component of an outer expression
/// or function.
#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStmt),
    If(IfStmt),       // condition, true, false
    ForIn(ForInStmt), // variable, iterable, body
    While(WhileStmt), // while, condition, body
    Declaration(Declaration),
    Definition(Definition),
    Expression(ExpressionStmt),
}

/// Enum representing the different declaration nodes (items) in the AST.
#[derive(Debug, Clone)]
pub enum Declaration {
    Import(ImportDecl),         // TODO
    Alias(AliasDecl),           // TODO
    Constant(ConstantDecl),     // TODO
    StaticItem(StaticItemDecl), // TODO
}

/// Enum representing the different item definitions nodes in the AST.
/// An item is a component of a package, organized by a set of modules.
#[derive(Debug, Clone)]
pub enum Definition {
    Module(ModuleDef),             // TODO
    Trait(TraitDef),               // TODO
    Enum(EnumDef),                 // TODO
    Struct(StructDef),             // TODO
    InherentImpl(InherentImplDef), // TODO
    TraitImpl(TraitImplDef),       // TODO
    Function(FunctionDef),         // TODO
}

// TODO: parse:
/// Enum representing the language's different types, which help to define a value's
/// memory interpretation and the appropriate operations that may be performed.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // primitives
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    U512,
    Byte,
    Bytes,
    H160,
    H256,
    H512,
    String, // `Vec<u8>`
    Char,
    Bool,

    UnitType, // ()

    // built-in collections
    Array,
    Tuple,

    UserDefined, // struct, enum, trait, alias, constant (paths / items)

    Function,
    Reference, //  `&Type` / `&mut Type`
    SelfType,
}

///////////////////////////////////////////////////////////////////////////
/// HELPER FUNCTIONS
///////////////////////////////////////////////////////////////////////////

/// Helper function to turn a slice into a `Bytes`.
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
