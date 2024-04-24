#![allow(dead_code)]

mod expression;
mod item;
mod statement;
mod types;

use crate::error::ParserErrorKind;

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
    Ref,
    Some,
    None,
    Ok,
    Err,
}

#[derive(Debug, Clone)]
pub enum InnerAttr {
    Contract,
    Library,
    Script,
    Unsafe,
}

#[derive(Debug, Clone)]
pub enum OuterAttr {
    Calldata,
    Constructor,
    Error,
    Event,
    Extern,
    Interface,
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
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    Exponentiation,
}

#[derive(Debug, Clone)]
pub enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

#[derive(Debug, Clone)]
pub enum CompoundAssignmentOp {
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModulusAssign,
}

#[derive(Debug, Clone)]
pub struct AssignmentOp(pub ());

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
    LeftAngledBracket,
    RightAngledBracket,
    Pipe,
    DblPipe,
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
    Negation(NegationExpr),
    Borrow(BorrowExpr),
    Dereference(DereferenceExpr),
    TypeCast(TypeCastExpr),
    Binary(BinaryExpr),
    Comparison(ComparisonExpr),
    Grouped(GroupedExpr),
    Range(RangeExpr),
    Assignment(AssignmentExpr),
    CompoundAssignment(CompoundAssignmentExpr),
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
pub enum ValueExpr {
    Literal(Literal),
    PathExpr(PathExpr),
    MethodCallExpr(MethodCallExpr),
    FieldAccessExpr(FieldAccessExpr),
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
    TupleIndexExpr(TupleIndexExpr),
    UnwrapExpr(UnwrapExpr),
    NegationExpr(NegationExpr),
    BorrowExpr(BorrowExpr),
    DereferenceExpr(DereferenceExpr),
    TypeCastExpr(TypeCastExpr),
    BinaryExpr(BinaryExpr),
    GroupedExpr(GroupedExpr),
    RangeExpr(RangeExpr),
    ClosureExpr(ClosureExpr),
    BlockExpr(BlockExpr),
    UnderscoreExpr(UnderscoreExpr),
    ArrayExpr(ArrayExpr),
    StructExpr(StructExpr),
    TupleStructExpr(TupleStructExpr),
    TupleExpr(TupleExpr),
    IfExpr(IfExpr),
    MatchExpr(MatchExpr),
    ForInExpr(ForInExpr),
    WhileExpr(WhileExpr),
    SomeExpr(SomeExpr),
    NoneExpr(NoneExpr),
    ResultExpr(ResultExpr),
}

impl TryFrom<Expression> for ValueExpr {
    type Error = ParserErrorKind;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match value {
            Expression::Literal(l) => Ok(ValueExpr::Literal(l)),
            Expression::Path(p) => Ok(ValueExpr::PathExpr(p)),
            Expression::MethodCall(mc) => Ok(ValueExpr::MethodCallExpr(mc)),
            Expression::FieldAccess(fa) => Ok(ValueExpr::FieldAccessExpr(fa)),
            Expression::Call(c) => Ok(ValueExpr::CallExpr(c)),
            Expression::Index(i) => Ok(ValueExpr::IndexExpr(i)),
            Expression::TupleIndex(ti) => Ok(ValueExpr::TupleIndexExpr(ti)),
            Expression::Unwrap(u) => Ok(ValueExpr::UnwrapExpr(u)),
            Expression::Negation(u) => Ok(ValueExpr::NegationExpr(u)),
            Expression::Borrow(b) => Ok(ValueExpr::BorrowExpr(b)),
            Expression::Dereference(d) => Ok(ValueExpr::DereferenceExpr(d)),
            Expression::TypeCast(tc) => Ok(ValueExpr::TypeCastExpr(tc)),
            Expression::Binary(b) => Ok(ValueExpr::BinaryExpr(b)),
            Expression::Grouped(g) => Ok(ValueExpr::GroupedExpr(g)),
            Expression::Range(r) => Ok(ValueExpr::RangeExpr(r)),
            Expression::Underscore(u) => Ok(ValueExpr::UnderscoreExpr(u)),
            Expression::Closure(c) => Ok(ValueExpr::ClosureExpr(c)),
            Expression::Array(a) => Ok(ValueExpr::ArrayExpr(a)),
            Expression::Tuple(t) => Ok(ValueExpr::TupleExpr(t)),
            Expression::Struct(s) => Ok(ValueExpr::StructExpr(s)),
            Expression::TupleStruct(ts) => Ok(ValueExpr::TupleStructExpr(ts)),
            Expression::Block(b) => Ok(ValueExpr::BlockExpr(b)),
            Expression::If(i) => Ok(ValueExpr::IfExpr(i)),
            Expression::Match(m) => Ok(ValueExpr::MatchExpr(m)),
            Expression::ForIn(fi) => Ok(ValueExpr::ForInExpr(fi)),
            Expression::While(w) => Ok(ValueExpr::WhileExpr(w)),
            Expression::SomeExpr(s) => Ok(ValueExpr::SomeExpr(s)),
            Expression::NoneExpr(n) => Ok(ValueExpr::NoneExpr(n)),
            Expression::ResultExpr(r) => Ok(ValueExpr::ResultExpr(r)),
            _ => Err(ParserErrorKind::TypeConversionError {
                type_a: "`Expression`".to_string(),
                type_b: "`ValueExpr`".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AssigneeExpr {
    Literal(Literal),
    PathExpr(PathExpr),
    MethodCallExpr(MethodCallExpr), // e.g., getter in a comparison expression
    FieldAccessExpr(FieldAccessExpr), // when on the LHS
    IndexExpr(IndexExpr),           // when on the LHS
    TupleIndexExpr(TupleIndexExpr), // when on the LHS
    BorrowExpr(BorrowExpr),
    GroupedExpr(Box<AssigneeExpr>),
    UnderscoreExpr(UnderscoreExpr),
    SliceExpr(Vec<AssigneeExpr>),
    TupleExpr(Option<Vec<AssigneeExpr>>),
    StructExpr(Vec<(Identifier, AssigneeExpr)>),
    TupleStructExpr(Vec<AssigneeExpr>),
}

impl TryFrom<Expression> for AssigneeExpr {
    type Error = ParserErrorKind;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match value {
            Expression::Literal(l) => Ok(AssigneeExpr::Literal(l)),
            Expression::Path(p) => Ok(AssigneeExpr::PathExpr(p)),
            Expression::MethodCall(mc) => Ok(AssigneeExpr::MethodCallExpr(mc)),
            Expression::FieldAccess(fa) => Ok(AssigneeExpr::FieldAccessExpr(fa)),
            Expression::Index(i) => Ok(AssigneeExpr::IndexExpr(i)),
            Expression::TupleIndex(ti) => Ok(AssigneeExpr::TupleIndexExpr(ti)),
            Expression::Borrow(b) => Ok(AssigneeExpr::BorrowExpr(b)),
            Expression::Grouped(g) => {
                let assignee_expression = AssigneeExpr::try_from(*g.expression)?;
                Ok(AssigneeExpr::GroupedExpr(Box::new(assignee_expression)))
            }
            Expression::Underscore(u) => Ok(AssigneeExpr::UnderscoreExpr(u)),
            Expression::Array(a) => {
                let mut assignee_expressions: Vec<AssigneeExpr> = Vec::new();
                a.elements_opt.map(|v| {
                    v.into_iter().for_each(|e| {
                        assignee_expressions.push(AssigneeExpr::try_from(e).expect(
                            "conversion error: unable to convert `Expression` to `AssigneeExpr`",
                        ))
                    })
                });

                Ok(AssigneeExpr::SliceExpr(assignee_expressions))
            }

            Expression::Tuple(t) => {
                let assignee_expressions = t.elements_opt.map(|te| {
                    let mut elements: Vec<Expression> = Vec::new();

                    te.elements.into_iter().for_each(|e| {
                        elements.push(e.0);
                    });

                    if let Some(e) = te.final_element_opt {
                        elements.push(*e)
                    }

                    elements
                        .into_iter()
                        .map(|e| {
                            AssigneeExpr::try_from(e).expect(
                            "conversion error: unable to convert `Expression` to `AssigneeExpr`",
                        )
                        })
                        .collect::<Vec<AssigneeExpr>>()
                });

                Ok(AssigneeExpr::TupleExpr(assignee_expressions))
            }

            Expression::Struct(s) => {
                let mut assignee_expressions: Vec<(Identifier, AssigneeExpr)> = Vec::new();

                s.fields_opt.map(|v| {
                    v.into_iter().for_each(|s| {
                        let value = AssigneeExpr::try_from(s.value).expect(
                            "conversion error: unable to convert `Expression` to `AssigneeExpr`",
                        );
                        assignee_expressions.push((s.name, value));
                    })
                });

                Ok(AssigneeExpr::StructExpr(assignee_expressions))
            }

            Expression::TupleStruct(ts) => {
                let mut assignee_expressions: Vec<AssigneeExpr> = Vec::new();
                ts.elements_opt.map(|v| {
                    v.into_iter().for_each(|e| {
                        assignee_expressions.push(AssigneeExpr::try_from(e).expect(
                            "conversion error: unable to convert `Expression` to `AssigneeExpr`",
                        ))
                    })
                });

                Ok(AssigneeExpr::TupleStructExpr(assignee_expressions))
            }

            _ => Err(ParserErrorKind::TypeConversionError {
                type_a: "`Expression`".to_string(),
                type_b: "`AssigneeExpr`".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Literal(Literal),
    IdentifierPatt {
        kw_ref_opt: Option<Keyword>,
        kw_mut_opt: Option<Keyword>,
        name: Identifier,
    },
    PathPatt(PathExpr),
    GroupedPatt(Box<Pattern>),
    RangePatt(RangeExpr),
    ArrayPatt(ArrayExpr),
    TuplePatt(TupleExpr),
    StructPatt(StructExpr),
    TupleStructPatt(TupleStructExpr),
    WildcardPatt(UnderscoreExpr),
    RestPatt {
        dbl_dot: RangeOp,
    },
}

impl TryFrom<Expression> for Pattern {
    type Error = ParserErrorKind;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match value {
            Expression::Literal(l) => Ok(Pattern::Literal(l)),
            Expression::Path(p) => Ok(Pattern::PathPatt(p)),
            Expression::Grouped(g) => Ok(Pattern::GroupedPatt(Box::new(Pattern::try_from(
                *g.expression,
            )?))),
            Expression::Range(r) => {
                if r.from_opt.is_none() && r.to_opt.is_none() {
                    Ok(Pattern::RestPatt { dbl_dot: r.op })
                } else {
                    Ok(Pattern::RangePatt(r))
                }
            }
            Expression::Array(a) => Ok(Pattern::ArrayPatt(a)),
            Expression::Tuple(t) => Ok(Pattern::TuplePatt(t)),
            Expression::Struct(s) => Ok(Pattern::StructPatt(s)),
            Expression::TupleStruct(ts) => Ok(Pattern::TupleStructPatt(ts)),
            Expression::Underscore(u) => Ok(Pattern::WildcardPatt(u)),

            _ => Err(ParserErrorKind::TypeConversionError {
                type_a: "`Expression`".to_string(),
                type_b: "`Pattern`".to_string(),
            }),
        }
    }
}

/// Enum representing the different statement AST nodes, which are built up of expressions.
/// A `Statement` is a component of a block, which is a component of an outer expression
/// or function.
#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStmt),
    Item(Item),
    Expression(Expression),
}

/// Enum representing the different item nodes in the AST.
/// An item is a component of a package, organized by a set of modules.
#[derive(Debug, Clone)]
pub enum Item {
    ImportDecl(ImportDecl),
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    StaticItemDecl(StaticItemDecl),
    ModuleDef(Box<ModuleItem>),
    TraitDef(TraitDef),
    EnumDef(EnumDef),
    StructDef(StructDef),
    InherentImplDef(InherentImplDef),
    TraitImplDef(TraitImplDef),
    FunctionDef(FunctionItem),
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
