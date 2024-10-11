//! ## AST
//!
//! Contains the different nodes that make up the abstract syntax tree (AST) and the tokens
//! within those nodes. The primary nodes are `Expression`, `Item` and `Statement`.

mod expression;
mod item;
mod pattern;
mod statement;
mod types;

use core::fmt;

use crate::{
    error::ParserErrorKind,
    semantic_analyser::{FormatObject, ToExpression},
    span::{Position, Span, Spanned},
};

pub(crate) use self::expression::*;
pub(crate) use self::item::*;
pub(crate) use self::pattern::*;
pub(crate) use self::statement::*;
pub(crate) use self::types::*;

///////////////////////////////////////////////////////////////////////////
// LITERAL
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different literals used in AST nodes.
#[derive(Clone, Hash, PartialEq, Eq)]
pub(crate) enum Literal {
    Int { value: Int, span: Span },
    UInt { value: UInt, span: Span },
    BigUInt { value: BigUInt, span: Span },
    Float { value: Float, span: Span },
    Byte { value: Byte, span: Span },
    Bytes { value: Bytes, span: Span },
    Hash { value: Hash, span: Span },
    Str { value: Str, span: Span },
    Char { value: Char, span: Span },
    Bool { value: Bool, span: Span },
}

impl Spanned for Literal {
    fn span(&self) -> Span {
        match self.clone() {
            Literal::Int { span, .. } => span,
            Literal::UInt { span, .. } => span,
            Literal::BigUInt { span, .. } => span,
            Literal::Float { span, .. } => span,
            Literal::Byte { span, .. } => span,
            Literal::Bytes { span, .. } => span,
            Literal::Hash { span, .. } => span,
            Literal::Str { span, .. } => span,
            Literal::Char { span, .. } => span,
            Literal::Bool { span, .. } => span,
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int { value, .. } => write!(f, "{}", value),
            Literal::UInt { value, .. } => write!(f, "{}", value),
            Literal::BigUInt { value, .. } => write!(f, "{}", value),
            Literal::Float { value, .. } => write!(f, "{}", value),
            Literal::Byte { value, .. } => write!(f, "{}", value),
            Literal::Bytes { value, .. } => write!(f, "{}", value.as_string()),
            Literal::Hash { value, .. } => write!(f, "{}", value),
            Literal::Str { value, .. } => {
                write!(f, "{}", value.as_string())
            }
            Literal::Char { value, .. } => write!(f, "{}", value),
            Literal::Bool { value, .. } => write!(f, "{}", value),
        }
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => f.debug_struct("Int").field("value", value).finish(),
            Self::UInt { value, .. } => f.debug_struct("UInt").field("value", value).finish(),
            Self::BigUInt { value, .. } => f.debug_struct("BigUInt").field("value", value).finish(),
            Self::Float { value, .. } => f.debug_struct("Float").field("value", value).finish(),
            Self::Byte { value, .. } => f.debug_struct("Byte").field("value", value).finish(),
            Self::Bytes { value, .. } => f.debug_struct("Bytes").field("value", value).finish(),
            Self::Hash { value, .. } => f.debug_struct("Hash").field("value", value).finish(),
            Self::Str { value, .. } => f.debug_struct("Str").field("value", value).finish(),
            Self::Char { value, .. } => f.debug_struct("Char").field("value", value).finish(),
            Self::Bool { value, .. } => f.debug_struct("Bool").field("value", value).finish(),
        }
    }
}

///////////////////////////////////////////////////////////////String////////////
// IDENTIFIER
///////////////////////////////////////////////////////////////////////////

/// Wrapper type, turning a `String` into an `Identifier`.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd)]
pub(crate) struct Identifier(String);

impl Identifier {
    pub(crate) fn to_type_path(&self) -> TypePath {
        self.clone().into()
    }
}

impl FormatObject for Identifier {}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier(value.to_string())
    }
}

impl From<&String> for Identifier {
    fn from(value: &String) -> Self {
        Identifier(value.to_owned())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

///////////////////////////////////////////////////////////////////////////
// KEYWORDS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different keywords used in AST nodes.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Keyword {
    Import,
    Module,
    Lib,
    SelfKeyword,
    Pub,
    Const,
    Static,
    Alias,
    Func,
    Struct,
    Enum,
    Trait,
    Impl,
    If,
    Match,
    For,
    In,
    While,
    Break,
    Continue,
    Where,
    Return,
    Let,
    Mut,
    Ref,
    Some,
    None,
    Ok,
    Err,
    Anonymous,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Import => write!(f, "import"),
            Keyword::Module => write!(f, "module"),
            Keyword::Lib => write!(f, "lib"),
            Keyword::SelfKeyword => write!(f, "self"),
            Keyword::Pub => write!(f, "pub"),
            Keyword::Const => write!(f, "const"),
            Keyword::Static => write!(f, "static"),
            Keyword::Alias => write!(f, "alias"),
            Keyword::Func => write!(f, "func"),
            Keyword::Struct => write!(f, "struct"),
            Keyword::Enum => write!(f, "enum"),
            Keyword::Trait => write!(f, "trait"),
            Keyword::Impl => write!(f, "impl"),
            Keyword::If => write!(f, "if"),
            Keyword::Match => write!(f, "match"),
            Keyword::For => write!(f, "for"),
            Keyword::In => write!(f, "in"),
            Keyword::While => write!(f, "while"),
            Keyword::Break => write!(f, "break"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Where => write!(f, "where"),
            Keyword::Return => write!(f, "return"),
            Keyword::Let => write!(f, "let"),
            Keyword::Ref => write!(f, "ref"),
            Keyword::Mut => write!(f, "mut"),
            Keyword::Some => write!(f, "Some"),
            Keyword::None => write!(f, "None"),
            Keyword::Ok => write!(f, "Ok"),
            Keyword::Err => write!(f, "Err"),
            Keyword::Anonymous => write!(f, ""),
        }
    }
}

/// Enum representing the different inner attributes used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum InnerAttr {
    Abstract,
    Contract,
    Interface,
    Library,
    Script,
    Unsafe,
}

/// Enum representing the different outer attributes used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
    LParen { position: Position },
    LBracket { position: Position },
    LBrace { position: Position },
    Pipe { position: Position },
    LAngleBracket { position: Position },
}

impl Delimiter {
    pub(crate) fn position(&self) -> Position {
        match self.clone() {
            Delimiter::LParen { position } => position,
            Delimiter::LBracket { position } => position,
            Delimiter::LBrace { position } => position,
            Delimiter::Pipe { position } => position,
            Delimiter::LAngleBracket { position } => position,
        }
    }
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Delimiter::LParen { .. } => write!(f, "("),
            Delimiter::LBracket { .. } => write!(f, "["),
            Delimiter::LBrace { .. } => write!(f, "{{"),
            Delimiter::Pipe { .. } => write!(f, "|"),
            Delimiter::LAngleBracket { .. } => write!(f, "<"),
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// PUNCTUATION
///////////////////////////////////////////////////////////////////////////

/// Unit struct representing the assignment operator (`=`) used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct AssignmentOp;

/// Enum representing the different binary operators used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::Modulus => write!(f, "%"),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::LogicalOr => write!(f, "||"),
            BinaryOp::BitwiseAnd => write!(f, "&"),
            BinaryOp::BitwiseOr => write!(f, "|"),
            BinaryOp::BitwiseXor => write!(f, "^"),
            BinaryOp::ShiftLeft => write!(f, "<<"),
            BinaryOp::ShiftRight => write!(f, ">>"),
            BinaryOp::Exponentiation => write!(f, "**"),
        }
    }
}

/// Unit struct representing the type cast operator (`as`).
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct TypeCastOp;

/// Enum representing the different comparison operators used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

impl fmt::Display for ComparisonOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ComparisonOp::Equal => write!(f, "=="),
            ComparisonOp::NotEqual => write!(f, "!="),
            ComparisonOp::LessThan => write!(f, "<"),
            ComparisonOp::LessEqual => write!(f, "<="),
            ComparisonOp::GreaterThan => write!(f, ">"),
            ComparisonOp::GreaterEqual => write!(f, ">="),
        }
    }
}

/// Enum representing the different compound assignment operators used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum CompoundAssignmentOp {
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModulusAssign,
}

impl fmt::Display for CompoundAssignmentOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompoundAssignmentOp::AddAssign => write!(f, "+="),
            CompoundAssignmentOp::SubtractAssign => write!(f, "-="),
            CompoundAssignmentOp::MultiplyAssign => write!(f, "*="),
            CompoundAssignmentOp::DivideAssign => write!(f, "/="),
            CompoundAssignmentOp::ModulusAssign => write!(f, "%="),
        }
    }
}

/// Unit struct representing the dereference operator (`*`).
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct DereferenceOp;

/// Enum representing the different range operators used in AST nodes.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) enum RangeOp {
    RangeExclusive, // `..`
    RangeInclusive, // `..=`
}

impl fmt::Display for RangeOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RangeOp::RangeExclusive => write!(f, ".."),
            RangeOp::RangeInclusive => write!(f, "..="),
        }
    }
}

/// Enum representing the different reference operators used in AST nodes (i.e., `&` and `&mut`).
#[derive(Default, Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) enum ReferenceOp {
    Borrow,        // `&`
    MutableBorrow, // `&mut`

    #[default]
    Owned,
}

impl fmt::Display for ReferenceOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReferenceOp::Borrow => write!(f, "&"),
            ReferenceOp::MutableBorrow => write!(f, "&mut "),
            ReferenceOp::Owned => write!(f, ""),
        }
    }
}

/// Unit struct representing the path wildcard operator (`::*`).
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct PathWildcard;

/// Enum representing the different unary operators used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum UnaryOp {
    Negate, // `-`
    Not,    // `!`
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

/// Unit struct representing the unwrap operator (`?`) used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct UnwrapOp;

///////////////////////////////////////////////////////////////////////////
// NODE GROUPS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different types of expression in the AST.
/// `Expression` nodes always produce or evaluate to a value and may have *side effects*.
/// Expressions can function differently in various contexts; i.e., they can act both as values
/// and as locations in memory. This distinction refers to **value expressions**
/// and **place expressions**. See additional expression type enumerations below for both cases.
#[derive(Clone, PartialEq, Eq)]
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
    TupleStruct(TupleStructExpr),
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

impl FormatObject for Expression {}

impl From<ValueExpr> for Expression {
    fn from(value: ValueExpr) -> Self {
        match value {
            ValueExpr::Literal(l) => Expression::Literal(l),
            ValueExpr::PathExpr(p) => Expression::Path(p),
            ValueExpr::MethodCallExpr(mc) => Expression::MethodCall(mc),
            ValueExpr::FieldAccessExpr(fa) => Expression::FieldAccess(fa),
            ValueExpr::CallExpr(c) => Expression::Call(c),
            ValueExpr::IndexExpr(i) => Expression::Index(i),
            ValueExpr::TupleIndexExpr(ti) => Expression::TupleIndex(ti),
            ValueExpr::UnwrapExpr(unw) => Expression::Unwrap(unw),
            ValueExpr::UnaryExpr(una) => Expression::Unary(una),
            ValueExpr::ReferenceExpr(r) => Expression::Reference(r),
            ValueExpr::DereferenceExpr(dr) => Expression::Dereference(dr),
            ValueExpr::TypeCastExpr(tc) => Expression::TypeCast(tc),
            ValueExpr::BinaryExpr(b) => Expression::Binary(b),
            ValueExpr::GroupedExpr(g) => Expression::Grouped(g),
            ValueExpr::RangeExpr(r) => Expression::Range(r),
            ValueExpr::ClosureExpr(c) => Expression::Closure(c),
            ValueExpr::UnderscoreExpr(u) => Expression::Underscore(u),
            ValueExpr::ArrayExpr(a) => Expression::Array(a),
            ValueExpr::TupleExpr(t) => Expression::Tuple(t),
            ValueExpr::StructExpr(s) => Expression::Struct(s),
            ValueExpr::TupleStructExpr(ts) => Expression::TupleStruct(ts),
            ValueExpr::MappingExpr(map) => Expression::Mapping(map),
            ValueExpr::BlockExpr(b) => Expression::Block(b),
            ValueExpr::IfExpr(i) => Expression::If(i),
            ValueExpr::MatchExpr(mat) => Expression::Match(mat),
            ValueExpr::ForInExpr(fi) => Expression::ForIn(fi),
            ValueExpr::WhileExpr(w) => Expression::While(w),
            ValueExpr::SomeExpr(s) => Expression::SomeExpr(s),
            ValueExpr::NoneExpr(n) => Expression::NoneExpr(n),
            ValueExpr::ResultExpr(r) => Expression::ResultExpr(r),
        }
    }
}

impl ToExpression for AssigneeExpr {}

impl From<AssigneeExpr> for Expression {
    fn from(value: AssigneeExpr) -> Self {
        match value {
            AssigneeExpr::Literal(l) => Expression::Literal(l),
            AssigneeExpr::PathExpr(p) => Expression::Path(p),
            AssigneeExpr::MethodCallExpr(mc) => Expression::MethodCall(mc),
            AssigneeExpr::FieldAccessExpr(fa) => Expression::FieldAccess(fa),
            AssigneeExpr::IndexExpr(i) => Expression::Index(i),
            AssigneeExpr::TupleIndexExpr(ti) => Expression::TupleIndex(ti),
            AssigneeExpr::ReferenceExpr(r) => Expression::Reference(r),
            AssigneeExpr::GroupedExpr {
                inner_expression,
                span,
            } => Expression::Grouped(GroupedExpr {
                inner_expression: Box::new(Expression::from(*inner_expression)),
                span,
            }),
            AssigneeExpr::UnderscoreExpr(u) => Expression::Underscore(u),
            AssigneeExpr::ArrayExpr { elements, span } => Expression::Array(ArrayExpr {
                elements_opt: {
                    let mut expressions: Vec<Expression> = Vec::new();

                    for ae in elements {
                        expressions.push(Expression::from(ae))
                    }

                    if expressions.is_empty() {
                        None
                    } else {
                        Some(expressions)
                    }
                },
                span,
            }),
            AssigneeExpr::TupleExpr { elements, span } => Expression::Tuple(TupleExpr {
                tuple_elements: {
                    let mut expressions: Vec<Expression> = Vec::new();

                    for ae in elements {
                        expressions.push(Expression::from(ae))
                    }

                    let final_element_opt = expressions.pop().map(|e| Box::new(e));

                    TupleElements {
                        elements: expressions,
                        final_element_opt,
                    }
                },
                span,
            }),
            AssigneeExpr::StructExpr {
                struct_path,
                fields,
                span,
            } => Expression::Struct(StructExpr {
                struct_path,
                struct_fields_opt: {
                    let mut struct_fields: Vec<StructField> = Vec::new();

                    for f in fields {
                        let field = StructField {
                            attributes_opt: f.attributes_opt,
                            field_name: f.field_name,
                            field_value: Box::new(Expression::from(*f.field_value)),
                        };

                        struct_fields.push(field)
                    }

                    if struct_fields.is_empty() {
                        None
                    } else {
                        Some(struct_fields)
                    }
                },
                span,
            }),
            AssigneeExpr::TupleStructExpr {
                struct_path,
                elements,
                span,
            } => Expression::TupleStruct(TupleStructExpr {
                struct_path,
                struct_elements_opt: {
                    let assignee_elements = elements
                        .into_iter()
                        .map(|ae| Expression::from(ae))
                        .collect::<Vec<_>>();

                    if !assignee_elements.is_empty() {
                        Some(assignee_elements)
                    } else {
                        None
                    }
                },
                span,
            }),
        }
    }
}

/// Enum representing expressions behaving in a **value expression** context.
/// A **value expression** is an expression that represents an actual value, as opposed
/// to a **place expression**, which represents a location in memory.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ValueExpr {
    Literal(Literal),
    PathExpr(PathExpr),
    MethodCallExpr(MethodCallExpr),
    FieldAccessExpr(FieldAccessExpr),
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
    TupleIndexExpr(TupleIndexExpr),
    UnwrapExpr(UnwrapExpr),
    UnaryExpr(UnaryExpr),
    ReferenceExpr(ReferenceExpr),
    DereferenceExpr(DereferenceExpr),
    TypeCastExpr(TypeCastExpr),
    BinaryExpr(BinaryExpr),
    GroupedExpr(GroupedExpr),
    RangeExpr(RangeExpr),
    ClosureExpr(ClosureExpr),
    UnderscoreExpr(UnderscoreExpr),
    ArrayExpr(ArrayExpr),
    TupleExpr(TupleExpr),
    StructExpr(StructExpr),
    TupleStructExpr(TupleStructExpr),
    MappingExpr(MappingExpr),
    BlockExpr(BlockExpr),
    IfExpr(IfExpr),
    MatchExpr(MatchExpr),
    ForInExpr(ForInExpr),
    WhileExpr(WhileExpr),
    SomeExpr(SomeExpr),
    NoneExpr(NoneExpr),
    ResultExpr(ResultExpr),
}

impl ToExpression for ValueExpr {}

impl Spanned for ValueExpr {
    fn span(&self) -> Span {
        match self.clone() {
            ValueExpr::Literal(l) => l.span(),
            ValueExpr::PathExpr(p) => p.span,
            ValueExpr::MethodCallExpr(mc) => mc.span,
            ValueExpr::FieldAccessExpr(fa) => fa.span,
            ValueExpr::CallExpr(c) => c.span,
            ValueExpr::IndexExpr(i) => i.span,
            ValueExpr::TupleIndexExpr(ti) => ti.span,
            ValueExpr::UnwrapExpr(unw) => unw.span,
            ValueExpr::UnaryExpr(una) => una.span,
            ValueExpr::ReferenceExpr(r) => r.span,
            ValueExpr::DereferenceExpr(dr) => dr.span,
            ValueExpr::TypeCastExpr(tc) => tc.span,
            ValueExpr::BinaryExpr(b) => b.span,
            ValueExpr::GroupedExpr(g) => g.span,
            ValueExpr::RangeExpr(r) => r.span,
            ValueExpr::ClosureExpr(c) => c.span,
            ValueExpr::UnderscoreExpr(u) => u.span,
            ValueExpr::ArrayExpr(a) => a.span,
            ValueExpr::TupleExpr(t) => t.span,
            ValueExpr::StructExpr(s) => s.span,
            ValueExpr::TupleStructExpr(ts) => ts.span,
            ValueExpr::MappingExpr(m) => m.span,
            ValueExpr::BlockExpr(b) => b.span,
            ValueExpr::IfExpr(i) => i.span,
            ValueExpr::MatchExpr(m) => m.span,
            ValueExpr::ForInExpr(fi) => fi.span,
            ValueExpr::WhileExpr(w) => w.span,
            ValueExpr::SomeExpr(s) => s.span,
            ValueExpr::NoneExpr(n) => n.span,
            ValueExpr::ResultExpr(r) => r.span,
        }
    }
}

impl TryFrom<Expression> for ValueExpr {
    type Error = ParserErrorKind;

    /// Check if an expression can act as a value expression and return the wrapped expression
    /// or throw an error.
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
            Expression::Unary(u) => Ok(ValueExpr::UnaryExpr(u)),
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
            Expression::TupleStruct(ts) => Ok(ValueExpr::TupleStructExpr(ts)),
            Expression::Mapping(m) => Ok(ValueExpr::MappingExpr(m)),
            Expression::Block(b) => Ok(ValueExpr::BlockExpr(b)),
            Expression::If(i) => Ok(ValueExpr::IfExpr(i)),
            Expression::Match(m) => Ok(ValueExpr::MatchExpr(m)),
            Expression::ForIn(fi) => Ok(ValueExpr::ForInExpr(fi)),
            Expression::While(w) => Ok(ValueExpr::WhileExpr(w)),
            Expression::SomeExpr(s) => Ok(ValueExpr::SomeExpr(s)),
            Expression::NoneExpr(n) => Ok(ValueExpr::NoneExpr(n)),
            Expression::ResultExpr(r) => Ok(ValueExpr::ResultExpr(r)),
            _ => Err(ParserErrorKind::UnexpectedExpression {
                expected: "value expression".to_string(),
                found: value.to_backtick_string(),
            }),
        }
    }
}

impl fmt::Display for ValueExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            ValueExpr::Literal(l) => write!(f, "{l}"),
            ValueExpr::PathExpr(p) => write!(f, "{p}"),
            ValueExpr::MethodCallExpr(mc) => write!(f, "{}", Expression::MethodCall(mc)),
            ValueExpr::FieldAccessExpr(fa) => write!(f, "{}", Expression::FieldAccess(fa)),
            ValueExpr::CallExpr(c) => write!(f, "{}", Expression::Call(c)),
            ValueExpr::IndexExpr(i) => write!(f, "{}", Expression::Index(i)),
            ValueExpr::TupleIndexExpr(ti) => write!(f, "{}", Expression::TupleIndex(ti)),
            ValueExpr::UnwrapExpr(unw) => write!(f, "{}", Expression::Unwrap(unw)),
            ValueExpr::UnaryExpr(una) => write!(f, "{}", Expression::Unary(una)),
            ValueExpr::ReferenceExpr(r) => write!(f, "{}", Expression::Reference(r)),
            ValueExpr::DereferenceExpr(d) => write!(f, "{}", Expression::Dereference(d)),
            ValueExpr::TypeCastExpr(tc) => write!(f, "{}", Expression::TypeCast(tc)),
            ValueExpr::BinaryExpr(b) => write!(f, "{}", Expression::Binary(b)),
            ValueExpr::GroupedExpr(g) => write!(f, "{}", Expression::Grouped(g)),
            ValueExpr::RangeExpr(r) => write!(f, "{}", Expression::Range(r)),
            ValueExpr::ClosureExpr(c) => write!(f, "{}", Expression::Closure(c)),
            ValueExpr::UnderscoreExpr(_) => write!(f, "_"),
            ValueExpr::ArrayExpr(a) => write!(f, "{}", Expression::Array(a)),
            ValueExpr::TupleExpr(t) => write!(f, "{}", Expression::Tuple(t)),
            ValueExpr::StructExpr(s) => write!(f, "{}", Expression::Struct(s)),
            ValueExpr::TupleStructExpr(ts) => write!(f, "{}", Expression::TupleStruct(ts)),
            ValueExpr::MappingExpr(m) => write!(f, "{}", Expression::Mapping(m)),
            ValueExpr::BlockExpr(b) => write!(f, "{}", Expression::Block(b)),
            ValueExpr::IfExpr(ifex) => write!(f, "{}", Expression::If(ifex)),
            ValueExpr::MatchExpr(m) => write!(f, "{}", Expression::Match(m)),
            ValueExpr::ForInExpr(fi) => write!(f, "{}", Expression::ForIn(fi)),
            ValueExpr::WhileExpr(w) => write!(f, "{}", Expression::While(w)),
            ValueExpr::SomeExpr(s) => write!(f, "{}", Expression::SomeExpr(s)),
            ValueExpr::NoneExpr(_) => write!(f, "None"),
            ValueExpr::ResultExpr(r) => write!(f, "{}", Expression::ResultExpr(r)),
        }
    }
}

/// Enum representing expressions behaving in a place / assignee expression context.
/// **Assignee expressions** usually occur on the left hand side of assignment expressions,
/// and cover all **place expressions**, the underscore expression, plus arrays
/// of assignee expressions, tuples of assignee expressions, and structs of assignee expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum AssigneeExpr {
    Literal(Literal),
    PathExpr(PathExpr),
    MethodCallExpr(MethodCallExpr), // e.g., getter in a comparison expression
    FieldAccessExpr(FieldAccessExpr), // when on the LHS
    IndexExpr(IndexExpr),           // when on the LHS
    TupleIndexExpr(TupleIndexExpr), // when on the LHS
    ReferenceExpr(ReferenceExpr),
    GroupedExpr {
        inner_expression: Box<AssigneeExpr>,
        span: Span,
    },
    UnderscoreExpr(UnderscoreExpr),
    ArrayExpr {
        elements: Vec<AssigneeExpr>,
        span: Span,
    },
    TupleExpr {
        elements: Vec<AssigneeExpr>,
        span: Span,
    },
    StructExpr {
        struct_path: PathExpr,
        fields: Vec<StructAssigneeExprField>,
        span: Span,
    },
    TupleStructExpr {
        struct_path: PathExpr,
        elements: Vec<AssigneeExpr>,
        span: Span,
    },
}

impl Spanned for AssigneeExpr {
    fn span(&self) -> Span {
        match self.clone() {
            AssigneeExpr::Literal(l) => l.span(),
            AssigneeExpr::PathExpr(p) => p.span,
            AssigneeExpr::MethodCallExpr(mc) => mc.span,
            AssigneeExpr::FieldAccessExpr(fa) => fa.span,
            AssigneeExpr::IndexExpr(i) => i.span,
            AssigneeExpr::TupleIndexExpr(ti) => ti.span,
            AssigneeExpr::ReferenceExpr(r) => r.span,
            AssigneeExpr::GroupedExpr { span, .. } => span,
            AssigneeExpr::UnderscoreExpr(u) => u.span,
            AssigneeExpr::ArrayExpr { span, .. } => span,
            AssigneeExpr::TupleExpr { span, .. } => span,
            AssigneeExpr::StructExpr { span, .. } => span,
            AssigneeExpr::TupleStructExpr { span, .. } => span,
        }
    }
}

impl TryFrom<Expression> for AssigneeExpr {
    type Error = ParserErrorKind;

    /// Check if an expression can act as an assignee expression and return the wrapped expression,
    /// or else throw an error.
    /// Also attempts to convert an `Expression` into an `AssigneeExpr`.
    /// E.g., An array expression can act as an assignee expression *if it contains elements
    /// of assignee expressions*. The same goes for struct and tuple assignee expressions.
    /// This function tries to convert elements / fields into assignee expressions in order
    /// to build collections of assignee expressions *that are assignee expressions themselves*.
    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match value {
            Expression::Literal(l) => Ok(AssigneeExpr::Literal(l)),
            Expression::Path(p) => Ok(AssigneeExpr::PathExpr(p)),
            Expression::MethodCall(mc) => Ok(AssigneeExpr::MethodCallExpr(mc)),
            Expression::FieldAccess(fa) => Ok(AssigneeExpr::FieldAccessExpr(fa)),
            Expression::Index(i) => Ok(AssigneeExpr::IndexExpr(i)),
            Expression::TupleIndex(ti) => Ok(AssigneeExpr::TupleIndexExpr(ti)),
            Expression::Reference(r) => Ok(AssigneeExpr::ReferenceExpr(r)),
            // `ReferenceExpr` is a placeholder `AssigneeExpr` where there is no equivalent
            Expression::Binary(b) => Ok(AssigneeExpr::ReferenceExpr(ReferenceExpr {
                reference_op: ReferenceOp::default(),
                expression: Box::new(Expression::Grouped(GroupedExpr {
                    inner_expression: Box::new(Expression::Binary(b.clone())),
                    span: b.span.clone(),
                })),
                span: b.span,
            })),
            Expression::Comparison(c) => Ok(AssigneeExpr::ReferenceExpr(ReferenceExpr {
                reference_op: ReferenceOp::default(),
                expression: Box::new(Expression::Grouped(GroupedExpr {
                    inner_expression: Box::new(Expression::Comparison(c.clone())),
                    span: c.span.clone(),
                })),
                span: c.span,
            })),
            Expression::Grouped(g) => {
                let inner_expression = AssigneeExpr::try_from(*g.inner_expression)?;
                Ok(AssigneeExpr::GroupedExpr {
                    inner_expression: Box::new(inner_expression),
                    span: g.span,
                })
            }
            Expression::Underscore(u) => Ok(AssigneeExpr::UnderscoreExpr(u)),
            Expression::Array(a) => {
                let mut elements: Vec<AssigneeExpr> = Vec::new();
                a.elements_opt.map(|v| {
                    v.into_iter().for_each(|e| {
                        elements.push(AssigneeExpr::try_from(e).expect(
                            "conversion error: unable to convert `Expression` into `AssigneeExpr`",
                        ))
                    })
                });

                Ok(AssigneeExpr::ArrayExpr {
                    elements,
                    span: a.span,
                })
            }

            Expression::Tuple(t) => {
                let elements = t
                    .tuple_elements
                    .elements
                    .into_iter()
                    .map(|te| {
                        AssigneeExpr::try_from(te).expect(
                            "conversion error: unable to convert `Expression` into `AssigneeExpr`",
                        )
                    })
                    .collect::<Vec<AssigneeExpr>>();

                Ok(AssigneeExpr::TupleExpr {
                    elements,
                    span: t.span,
                })
            }

            Expression::Struct(s) => {
                let mut fields: Vec<StructAssigneeExprField> = Vec::new();

                s.struct_fields_opt.map(|v| {
                    v.into_iter().for_each(|s| {
                        let attributes_opt = s.attributes_opt;
                        let field_value = AssigneeExpr::try_from(*s.field_value).expect(
                            "conversion error: unable to convert `Expression` into `AssigneeExpr`",
                        );

                        let struct_assignee_expr_field = StructAssigneeExprField {
                            attributes_opt,
                            field_name: s.field_name,
                            field_value: Box::new(field_value),
                        };
                        fields.push(struct_assignee_expr_field);
                    })
                });

                Ok(AssigneeExpr::StructExpr {
                    struct_path: s.struct_path,
                    fields,
                    span: s.span,
                })
            }

            Expression::TupleStruct(ts) => {
                let mut elements: Vec<AssigneeExpr> = Vec::new();

                ts.struct_elements_opt.map(|v| {
                    v.into_iter().for_each(|e| {
                        let element = AssigneeExpr::try_from(e).expect(
                            "conversion error: unable to convert `Expression` into `AssigneeExpr`",
                        );
                        elements.push(element);
                    })
                });

                Ok(AssigneeExpr::TupleStructExpr {
                    struct_path: ts.struct_path,
                    elements,
                    span: ts.span,
                })
            }

            _ => Err(ParserErrorKind::UnexpectedExpression {
                expected: "assignee expression".to_string(),
                found: value.to_backtick_string(),
            }),
        }
    }
}

impl fmt::Display for AssigneeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            AssigneeExpr::Literal(l) => write!(f, "{l}"),
            AssigneeExpr::PathExpr(p) => write!(f, "{p}"),
            AssigneeExpr::MethodCallExpr(mc) => write!(f, "{}", Expression::MethodCall(mc)),
            AssigneeExpr::FieldAccessExpr(fa) => write!(f, "{}", Expression::FieldAccess(fa)),
            AssigneeExpr::IndexExpr(i) => write!(f, "{}", Expression::Index(i)),
            AssigneeExpr::TupleIndexExpr(ti) => write!(f, "{}", Expression::TupleIndex(ti)),
            AssigneeExpr::ReferenceExpr(r) => write!(f, "{}", Expression::Reference(r)),
            AssigneeExpr::GroupedExpr {
                inner_expression, ..
            } => write!(f, "({})", *inner_expression),
            AssigneeExpr::UnderscoreExpr(_) => write!(f, "_"),
            AssigneeExpr::ArrayExpr { elements, .. } => write!(f, "[ {elements:?} ]"),
            AssigneeExpr::TupleExpr { elements, .. } => write!(f, "( {elements:?} )"),
            AssigneeExpr::StructExpr {
                struct_path,
                fields,
                ..
            } => write!(f, "{} {{ {:?} }}", struct_path, fields),
            AssigneeExpr::TupleStructExpr {
                struct_path,
                elements,
                ..
            } => write!(f, "{}({:?})", struct_path, elements),
        }
    }
}

/// Enum representing patterns, which are syntactically similar to `Expression`.
/// Patterns are used to match values against structures, as well as within
/// variable declarations and as function parameters.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Pattern {
    LiteralPatt(LiteralPatt),
    IdentifierPatt(IdentifierPatt),
    PathPatt(PathPatt),
    ReferencePatt(ReferencePatt),
    GroupedPatt(GroupedPatt),
    RangePatt(RangePatt),
    TuplePatt(TuplePatt),
    StructPatt(StructPatt),
    TupleStructPatt(TupleStructPatt),
    WildcardPatt(WildcardPatt),
    RestPatt(RestPatt),
    OrPatt(OrPatt),
    SomePatt(SomePatt),
    NonePatt(NonePatt),
    ResultPatt(ResultPatt),
}

/// Enum representing the different statement AST nodes, which are built up of expressions.
/// A statement is a component of a block, which is a component of an outer expression
/// or function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Statement {
    Let(LetStmt),
    Item(Item),
    Expression(Expression),
}

/// Enum representing the different item nodes in the AST.
/// An item is a component of a library, organized by a set of modules.
#[derive(Clone, PartialEq, Eq)]
pub(crate) enum Item {
    ImportDecl(ImportDecl),
    AliasDecl(AliasDecl),
    ConstantDecl(ConstantDecl),
    StaticVarDecl(StaticVarDecl),
    ModuleItem(ModuleItem),
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
#[derive(Clone, PartialEq, Eq)]
pub(crate) enum Type {
    // primitives
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    U256,
    U512,
    F32,
    F64,
    Byte,
    B2,
    B4,
    B8,
    B16,
    B32,
    H160,
    H256,
    H512,
    Str,
    Char,
    Bool,

    UnitType(UnitType), // ()

    GroupedType(Box<Type>),

    // built-in collections
    Array {
        element_type: Box<Type>,
        num_elements: UInt,
    },

    Tuple(Vec<Type>),

    UserDefined(TypePath), // struct, enum, trait, alias, constant (paths / items)

    FunctionPtr(FunctionPtr),

    Reference {
        reference_op: ReferenceOp, // `&` or `&mut`
        inner_type: Box<Type>,
    },

    SelfType(SelfType),

    InferredType(InferredType),

    Vec {
        element_type: Box<Type>,
    },

    Mapping {
        key_type: Box<Type>,
        value_type: Box<Type>,
    },

    Option {
        inner_type: Box<Type>,
    },

    Result {
        ok_type: Box<Type>,
        err_type: Box<Type>,
    },

    Generic(GenericParam),
}
