//! # AST
//!
//! Contains the different nodes that make up the abstract syntax tree (AST) and the tokens
//! within those nodes. The primary nodes are `Expression`, `Item` and `Statement`.

mod expression;
mod item;
mod pattern;
mod statement;
mod types;

use crate::error::ParserErrorKind;

pub(crate) use self::{expression::*, item::*, pattern::*, statement::*, types::*};

///////////////////////////////////////////////////////////////////////////
// LITERAL
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different literals used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
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
// IDENTIFIER
///////////////////////////////////////////////////////////////////////////

/// Wrapper type, turning a `String` into an `Identifier`.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Identifier(pub String);

///////////////////////////////////////////////////////////////////////////
// KEYWORDS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different keywords used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Keyword {
    Import,
    Module,
    Package,
    SelfKeyword,
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
    For,
    In,
    While,
    Break,
    Continue,
    Return,
    Let,
    Mut,
    Ref,
    Some,
    None,
    Ok,
    Err,
}

/// Enum representing the different inner attributes used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InnerAttr {
    Abstract,
    Contract,
    Interface,
    Library,
    Script,
    Unsafe,
}

/// Enum representing the different outer attributes used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OuterAttr {
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
// DELIMITERS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different delimiters used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Delimiter {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Pipe,
}

///////////////////////////////////////////////////////////////////////////
// PUNCTUATION
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different unary operators used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnaryOp {
    Negate, // `-`
    Not,    // `!`
}

/// Enum representing the different reference operators used in AST nodes (i.e., `&` and `&mut`).
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ReferenceOp {
    Borrow,        // `&`
    MutableBorrow, // `&mut`
}

/// Unit struct representing the dereference operator (`*`).
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct DereferenceOp;

/// Enum representing the different binary operators used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinaryOp {
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

/// Enum representing the different comparison operators used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

/// Enum representing the different compound assignment operators used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CompoundAssignmentOp {
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModulusAssign,
}

/// Unit struct representing the assignment operator (`=`) used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AssignmentOp;

/// Unit struct representing the unwrap operator (`?`) used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UnwrapOp;

/// Enum representing the different range operators used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RangeOp {
    RangeExclusive, // `..`
    RangeInclusive, // `..=`
}

/// Enum representing the different separators used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Separator {
    Comma,
    ColonColonAsterisk,
}

///////////////////////////////////////////////////////////////////////////
// NODE GROUPS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different types of expression in the AST.
/// `Expression` nodes always produce or evaluate to a value and may have side effects.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expression {
    Literal(Literal),
    Path(PathExpr),
    MethodCall(MethodCallExpr),
    FieldAccess(FieldAccessExpr),
    Call(CallExpr),
    Index(IndexExpr),
    TupleIndex(TupleIndexExpr),
    Unwrap(UnwrapExpr),
    Unary(UnaryExpr),
    Reference(ReferenceExpr),
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
    // TupleStruct(TupleStructExpr),
    Mapping(MappingExpr),
    Block(BlockExpr),
    If(IfExpr),       // condition, true, false
    Match(MatchExpr), // scrutinee, body
    ForIn(ForInExpr), // variable, iterable, body
    While(WhileExpr), // while, condition, body
    SomeExpr(SomeExpr),
    NoneExpr(NoneExpr),
    ResultExpr(ResultExpr),
}

/// Enum representing value type expressions, which are subsets of `Expression`.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ValueExpr {
    Literal(Literal),
    PathExpr(PathExpr),
    MethodCallExpr(MethodCallExpr),
    FieldAccessExpr(FieldAccessExpr),
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
    TupleIndexExpr(TupleIndexExpr),
    UnwrapExpr(UnwrapExpr),
    NegationExpr(UnaryExpr),
    ReferenceExpr(ReferenceExpr),
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
    MappingExpr(MappingExpr),
    // TupleStructExpr(TupleStructExpr),
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
            Expression::Unary(u) => Ok(ValueExpr::NegationExpr(u)),
            Expression::Reference(b) => Ok(ValueExpr::ReferenceExpr(b)),
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
            Expression::Mapping(m) => Ok(ValueExpr::MappingExpr(m)),
            // Expression::TupleStruct(ts) => Ok(ValueExpr::TupleStructExpr(ts)),
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

/// Enum representing assignee type expressions, which are subsets of `Expression`.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AssigneeExpr {
    Literal(Literal),
    PathExpr(PathExpr),
    MethodCallExpr(MethodCallExpr), // e.g., getter in a comparison expression
    FieldAccessExpr(FieldAccessExpr), // when on the LHS
    IndexExpr(IndexExpr),           // when on the LHS
    TupleIndexExpr(TupleIndexExpr), // when on the LHS
    ReferenceExpr(ReferenceExpr),
    GroupedExpr(Box<AssigneeExpr>),
    UnderscoreExpr(UnderscoreExpr),
    SliceExpr(Vec<AssigneeExpr>),
    TupleExpr(Vec<AssigneeExpr>),
    StructExpr(Vec<StructAssigneeExprField>),
    // TupleStructExpr(Vec<AssigneeExpr>),
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
            Expression::Reference(b) => Ok(AssigneeExpr::ReferenceExpr(b)),
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
                            "conversion error: unable to convert `Expression` into `AssigneeExpr`",
                        ))
                    })
                });

                Ok(AssigneeExpr::SliceExpr(assignee_expressions))
            }

            Expression::Tuple(t) => {
                let assignee_expressions = t
                    .tuple_elements
                    .elements
                    .into_iter()
                    .map(|te| {
                        AssigneeExpr::try_from(te.0).expect(
                            "conversion error: unable to convert `Expression` into `AssigneeExpr`",
                        )
                    })
                    .collect::<Vec<AssigneeExpr>>();

                Ok(AssigneeExpr::TupleExpr(assignee_expressions))
            }

            Expression::Struct(s) => {
                let mut assignee_expressions: Vec<StructAssigneeExprField> = Vec::new();

                s.fields_opt.map(|v| {
                    v.into_iter().for_each(|s| {
                        let attributes_opt = s.attributes_opt;
                        let field_value = AssigneeExpr::try_from(s.field_value).expect(
                            "conversion error: unable to convert `Expression` into `AssigneeExpr`",
                        );

                        let struct_assignee_expr_field = StructAssigneeExprField {
                            attributes_opt,
                            field_name: s.field_name,
                            field_value,
                        };
                        assignee_expressions.push(struct_assignee_expr_field);
                    })
                });

                Ok(AssigneeExpr::StructExpr(assignee_expressions))
            }

            // Expression::TupleStruct(ts) => {
            //     let mut assignee_expressions: Vec<AssigneeExpr> = Vec::new();
            //     ts.elements_opt.map(|v| {
            //         v.into_iter().for_each(|e| {
            //             assignee_expressions.push(AssigneeExpr::try_from(e).expect(
            //                 "conversion error: unable to convert `Expression` into `AssigneeExpr`",
            //             ))
            //         })
            //     });

            //     Ok(AssigneeExpr::TupleStructExpr(assignee_expressions))
            // }
            _ => Err(ParserErrorKind::TypeConversionError {
                type_a: "`Expression`".to_string(),
                type_b: "`AssigneeExpr`".to_string(),
            }),
        }
    }
}

/// Enum representing patterns, which are syntactically similar to `Expression`.
/// Patterns are used to match values against structures, as well as within
/// variable declarations and as function parameters.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Pattern {
    Literal(Literal),
    IdentifierPatt(IdentifierPatt),
    PathPatt(PathPatt),
    ReferencePatt(ReferencePatt),
    GroupedPatt(GroupedPatt),
    RangePatt(RangePatt),
    TuplePatt(TuplePatt),
    StructPatt(StructPatt),
    // TupleStructPatt {
    //     name: Identifier,
    //     elements_opt: Option<Vec<Pattern>>,
    // },
    WildcardPatt(WildcardPatt),
    RestPatt(RestPatt),
    SomePatt(SomePatt),
    NonePatt(NonePatt),
    ResultPatt(ResultPatt),
}

/// Enum representing the different statement AST nodes, which are built up of expressions.
/// A statement is a component of a block, which is a component of an outer expression
/// or function.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Statement {
    Let(LetStmt),
    Item(Item),
    Expression(Expression),
}

/// Enum representing the different item nodes in the AST.
/// An item is a component of a package, organized by a set of modules.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Item {
    ImportDecl(ImportDecl),
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    StaticItemDecl(StaticItemDecl),
    ModuleItem(Box<ModuleItem>),
    TraitDef(TraitDef),
    EnumDef(EnumDef),
    StructDef(StructDef),
    TupleStructDef(TupleStructDef),
    InherentImplDef(InherentImplDef),
    TraitImplDef(TraitImplDef),
    FunctionItem(FunctionItem),
}

/// Enum representing the language's different types, which help to define a value's
/// memory interpretation and the appropriate operations that may be performed.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
    // primitives
    I32(Int),
    I64(Int),
    I128(Int),
    U8(UInt),
    U16(UInt),
    U32(UInt),
    U64(UInt),
    U128(UInt),
    U256(BigUInt),
    U512(BigUInt),
    Byte(Byte),
    B2(Bytes),
    B4(Bytes),
    B8(Bytes),
    B16(Bytes),
    B32(Bytes),
    H160(Hash),
    H256(Hash),
    H512(Hash),
    Str(Str),
    Char(Char),
    Bool(Bool),

    UnitType(Unit), // ()

    GroupedType(Box<Type>),

    // built-in collections
    Array {
        element_type: Box<Type>,
        num_elements: UInt,
    },
    Tuple(Vec<Type>),

    UserDefined(PathType), // struct, enum, trait, alias, constant (paths / items)

    FunctionPtr(FunctionPtr),
    Reference {
        reference_op: ReferenceOp, // `&` or `&mut`
        inner_type: Box<Type>,
    },
    SelfType(SelfType),

    InferredType(InferredType),

    Vec(Box<Type>),
    Mapping {
        key_type: Box<Type>,
        value_type: Box<Type>,
    },

    Option {
        inner_type: Box<Type>,
    },
    Result {
        ok: Box<Type>,
        err: Box<Type>,
    },
}
