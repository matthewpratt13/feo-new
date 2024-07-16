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
    span::{Position, Span, Spanned},
};

pub(crate) use self::{expression::*, item::*, pattern::*, statement::*, types::*};

///////////////////////////////////////////////////////////////////////////
// LITERAL
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different literals used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Literal {
    Int { value: Int, span: Span },
    UInt { value: UInt, span: Span },
    BigUInt { value: BigUInt, span: Span },
    Float { value: Float, span: Span },
    Byte { value: Byte, span: Span },
    Bytes { value: Bytes, span: Span },
    Hash { value: types::Hash, span: Span },
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
            Literal::Bytes { value, .. } => write!(f, "{}", value),
            Literal::Hash { value, .. } => write!(f, "{}", value),
            Literal::Str { value, .. } => write!(f, "{}", value),
            Literal::Char { value, .. } => write!(f, "{}", value),
            Literal::Bool { value, .. } => write!(f, "{}", value),
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// IDENTIFIER
///////////////////////////////////////////////////////////////////////////

/// Wrapper type, turning a `String` into an `Identifier`.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd)]
pub(crate) struct Identifier(String);

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
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
    Return,
    Let,
    Mut,
    Ref,
    Some,
    None,
    Ok,
    Err,
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
            Keyword::Return => write!(f, "return"),
            Keyword::Let => write!(f, "let"),
            Keyword::Ref => write!(f, "ref"),
            Keyword::Mut => write!(f, "mut"),
            Keyword::Some => write!(f, "Some"),
            Keyword::None => write!(f, "None"),
            Keyword::Ok => write!(f, "Ok"),
            Keyword::Err => write!(f, "Err"),
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
}

impl Delimiter {
    pub(crate) fn position(&self) -> Position {
        match self.clone() {
            Delimiter::LParen { position } => position,
            Delimiter::LBracket { position } => position,
            Delimiter::LBrace { position } => position,
            Delimiter::Pipe { position } => position,
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
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) enum ReferenceOp {
    Borrow,        // `&`
    MutableBorrow, // `&mut`
}

impl fmt::Display for ReferenceOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReferenceOp::Borrow => write!(f, "&"),
            ReferenceOp::MutableBorrow => write!(f, "&mut"),
        }
    }
}

/// Enum representing the different separators used in AST nodes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Separator {
    Comma,              // used in tuples
    ColonColonAsterisk, // path wildcard terminator
}

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
#[derive(Debug, Clone, PartialEq, Eq)]
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

impl Spanned for Expression {
    fn span(&self) -> crate::span::Span {
        match self.clone() {
            Expression::Literal(l) => l.span(),
            Expression::Path(p) => p.span,
            Expression::MethodCall(mc) => mc.span,
            Expression::FieldAccess(fa) => fa.span,
            Expression::Call(c) => c.span,
            Expression::Index(i) => i.span,
            Expression::TupleIndex(ti) => ti.span,
            Expression::Unwrap(unw) => unw.span,
            Expression::Unary(una) => una.span,
            Expression::Reference(r) => r.span,
            Expression::Dereference(de) => de.span,
            Expression::TypeCast(tc) => tc.span,
            Expression::Binary(b) => b.span,
            Expression::Comparison(c) => c.span,
            Expression::Grouped(g) => g.span,
            Expression::Range(r) => r.span,
            Expression::Assignment(a) => a.span,
            Expression::CompoundAssignment(ca) => ca.span,
            Expression::Return(r) => r.span,
            Expression::Break(b) => b.span,
            Expression::Continue(c) => c.span,
            Expression::Underscore(u) => u.span,
            Expression::Closure(c) => c.span,
            Expression::Array(a) => a.span,
            Expression::Tuple(t) => t.span,
            Expression::Struct(s) => s.span,
            Expression::Mapping(m) => m.span,
            Expression::Block(b) => b.span,
            Expression::If(i) => i.span,
            Expression::Match(m) => m.span,
            Expression::ForIn(fi) => fi.span,
            Expression::While(w) => w.span,
            Expression::SomeExpr(s) => s.span,
            Expression::NoneExpr(n) => n.span,
            Expression::ResultExpr(r) => r.span,
        }
    }
}

impl From<Expression> for PathExpr {
    fn from(value: Expression) -> Self {
        match value {
            Expression::Path(p) => p,
            e => PathExpr {
                path_root: PathRoot::Identifier(Identifier::from("")),
                tree_opt: None,
                span: e.span(),
            },
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            Expression::Literal(l) => match l {
                Literal::Int { value, .. } => write!(f, "{}", value),
                Literal::UInt { value, .. } => write!(f, "{}", value),
                Literal::BigUInt { value, .. } => write!(f, "{}", value),
                Literal::Float { value, .. } => write!(f, "{}", value),
                Literal::Byte { value, .. } => write!(f, "{}", value),
                Literal::Bytes { value, .. } => write!(f, "{}", value),
                Literal::Hash { value, .. } => write!(f, "{}", value),
                Literal::Str { value, .. } => write!(f, "{}", value),
                Literal::Char { value, .. } => write!(f, "{}", value),
                Literal::Bool { value, .. } => write!(f, "{}", value),
            },
            Expression::Path(pth) => write!(f, "{}", PathType::from(pth)),
            Expression::MethodCall(mc) => write!(f, "{}.{}()", mc.receiver, mc.method_name),
            Expression::FieldAccess(fa) => write!(f, "{}.{}", fa.object, fa.field_name),
            Expression::Call(cal) => write!(
                f,
                "{}({:?})",
                cal.callee,
                cal.args_opt.unwrap_or(Vec::new())
            ),
            Expression::Index(i) => write!(f, "{}[{}]", *i.array, *i.index),
            Expression::TupleIndex(ti) => write!(f, "{}.{}", *ti.tuple, ti.index),
            Expression::Unwrap(unw) => write!(f, "{}?", *unw.value_expr),
            Expression::Unary(una) => write!(f, "{}{}", una.unary_op, *una.value_expr),
            Expression::Reference(r) => write!(f, "{}{}", r.reference_op, r.expression),
            Expression::Dereference(dr) => write!(f, "*{}", dr.assignee_expr),
            Expression::TypeCast(tc) => write!(f, "{} as {}", tc.value, tc.new_type),
            Expression::Binary(bin) => write!(f, "{} {} {}", bin.lhs, bin.binary_op, bin.rhs),
            Expression::Comparison(cmp) => {
                write!(f, "{} {} {}", cmp.lhs, cmp.comparison_op, cmp.rhs)
            }
            Expression::Grouped(grp) => write!(f, "({})", *grp.inner_expression),
            Expression::Range(rng) => write!(
                f,
                "{}{}{}",
                rng.from_expr_opt
                    .unwrap_or(Box::new(AssigneeExpr::Literal(Literal::Int {
                        value: Int::I64(i64::MIN),
                        span: rng.span.clone()
                    }))),
                rng.range_op,
                rng.to_expr_opt
                    .unwrap_or(Box::new(AssigneeExpr::Literal(Literal::BigUInt {
                        value: BigUInt::U512(U512::MAX),
                        span: rng.span
                    })))
            ),
            Expression::Assignment(asn) => write!(f, "{} = {}", asn.lhs, asn.rhs),
            Expression::CompoundAssignment(casn) => write!(
                f,
                "{} {} {}",
                casn.lhs, casn.compound_assignment_op, casn.rhs
            ),
            Expression::Return(ret) => write!(
                f,
                "return {}",
                ret.expression_opt
                    .unwrap_or(Box::new(Expression::Tuple(TupleExpr {
                        tuple_elements: TupleElements {
                            elements: Vec::new(),
                            final_element_opt: None
                        },
                        span: ret.span,
                    })))
            ),
            Expression::Break(_) => write!(f, "break"),
            Expression::Continue(_) => write!(f, "continue"),
            Expression::Underscore(_) => write!(f, "_"),
            Expression::Closure(clo) => write!(
                f,
                "({:?}) -> {} {}",
                clo.closure_params,
                clo.return_type_opt
                    .clone()
                    .unwrap_or(Box::new(Type::UnitType(Unit))),
                clo.body_expression
            ),
            Expression::Array(arr) => write!(f, "[ {:?} ]", arr.elements_opt),
            Expression::Tuple(tup) => write!(f, "( {} )", tup.tuple_elements),
            Expression::Struct(strc) => {
                write!(
                    f,
                    "{} {{ {:?} }}",
                    strc.struct_path,
                    strc.struct_fields_opt.unwrap_or(Vec::new())
                )
            }
            Expression::Mapping(map) => write!(f, "{{ {:?} }}", map.pairs_opt),
            Expression::Block(blk) => {
                write!(f, "{{ {:?} }}", blk.statements_opt.unwrap_or(Vec::new()))
            }
            Expression::If(ifex) => write!(
                f,
                "if {} {}{}{}",
                Expression::Grouped(*ifex.condition),
                Expression::Block(*ifex.if_block),
                {
                    if let Some(e) = ifex.else_if_blocks_opt {
                        format!(" else {:?}", e)
                    } else {
                        "".to_string()
                    }
                },
                {
                    if let Some(e) = ifex.trailing_else_block_opt {
                        format!(" else {}", Expression::Block(e))
                    } else {
                        "".to_string()
                    }
                }
            ),
            Expression::Match(mat) => write!(
                f,
                "match {} {{ {}, {} }}",
                mat.scrutinee,
                {
                    if let Some(m) = mat.match_arms_opt {
                        format!("{:?}", m)
                    } else {
                        "".to_string()
                    }
                },
                *mat.final_arm
            ),
            Expression::ForIn(fi) => write!(
                f,
                "for {} in {} {}",
                fi.pattern,
                fi.iterator,
                Expression::Block(fi.block)
            ),
            Expression::While(whl) => write!(
                f,
                "while {} {}",
                Expression::Grouped(*whl.condition),
                Expression::Block(whl.block)
            ),
            Expression::SomeExpr(som) => write!(f, "Some{}", Expression::Grouped(*som.expression)),
            Expression::NoneExpr(_) => write!(f, "None"),
            Expression::ResultExpr(res) => write!(f, "{}", {
                match res.kw_ok_or_err {
                    Keyword::Ok => format!("Ok{}", Expression::Grouped(*res.expression)),
                    Keyword::Err => format!("Err{}", Expression::Grouped(*res.expression)),
                    _ => "".to_string(),
                }
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
                found: format!("`{}`", value),
            }),
        }
    }
}

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

impl fmt::Display for ValueExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            ValueExpr::Literal(l) => write!(f, "{}", Expression::Literal(l)),
            ValueExpr::PathExpr(p) => write!(f, "{}", Expression::Path(p)),
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
            ValueExpr::UnderscoreExpr(und) => write!(f, "{}", Expression::Underscore(und)),
            ValueExpr::ArrayExpr(a) => write!(f, "{}", Expression::Array(a)),
            ValueExpr::TupleExpr(t) => write!(f, "{}", Expression::Tuple(t)),
            ValueExpr::StructExpr(s) => write!(f, "{}", Expression::Struct(s)),
            ValueExpr::MappingExpr(m) => write!(f, "{}", Expression::Mapping(m)),
            ValueExpr::BlockExpr(b) => write!(f, "{}", Expression::Block(b)),
            ValueExpr::IfExpr(ifex) => write!(f, "{}", Expression::If(ifex)),
            ValueExpr::MatchExpr(m) => write!(f, "{}", Expression::Match(m)),
            ValueExpr::ForInExpr(fi) => write!(f, "{}", Expression::ForIn(fi)),
            ValueExpr::WhileExpr(w) => write!(f, "{}", Expression::While(w)),
            ValueExpr::SomeExpr(s) => write!(f, "{}", Expression::SomeExpr(s)),
            ValueExpr::NoneExpr(n) => write!(f, "{}", Expression::NoneExpr(n)),
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
            Expression::Reference(b) => Ok(AssigneeExpr::ReferenceExpr(b)),
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

            _ => Err(ParserErrorKind::UnexpectedExpression {
                expected: "assignee expression".to_string(),
                found: format!("{}", value),
            }),
        }
    }
}

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
        }
    }
}

impl fmt::Display for AssigneeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            AssigneeExpr::Literal(l) => write!(f, "{}", Expression::Literal(l)),
            AssigneeExpr::PathExpr(p) => write!(f, "{}", Expression::Path(p)),
            AssigneeExpr::MethodCallExpr(mc) => write!(f, "{}", Expression::MethodCall(mc)),
            AssigneeExpr::FieldAccessExpr(fa) => write!(f, "{}", Expression::FieldAccess(fa)),
            AssigneeExpr::IndexExpr(i) => write!(f, "{}", Expression::Index(i)),
            AssigneeExpr::TupleIndexExpr(ti) => write!(f, "{}", Expression::TupleIndex(ti)),
            AssigneeExpr::ReferenceExpr(r) => write!(f, "{}", Expression::Reference(r)),
            AssigneeExpr::GroupedExpr {
                inner_expression, ..
            } => write!(f, "({})", *inner_expression),
            AssigneeExpr::UnderscoreExpr(und) => write!(f, "{}", Expression::Underscore(und)),
            AssigneeExpr::ArrayExpr { elements, .. } => write!(f, "[ {:?} ]", elements),
            AssigneeExpr::TupleExpr { elements, .. } => write!(f, "( {:?} )", elements),
            AssigneeExpr::StructExpr {
                struct_path,
                fields,
                ..
            } => write!(f, "{} {{ {:#?} }}", struct_path, fields),
        }
    }
}

/// Enum representing patterns, which are syntactically similar to `Expression`.
/// Patterns are used to match values against structures, as well as within
/// variable declarations and as function parameters.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum Pattern {
    Literal(Literal),
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
    SomePatt(SomePatt),
    NonePatt(NonePatt),
    ResultPatt(ResultPatt),
}

impl TryFrom<Expression> for Pattern {
    type Error = ParserErrorKind;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match value {
            Expression::Literal(l) => Ok(Pattern::Literal(l)),
            Expression::Path(p) => Ok(Pattern::PathPatt(PathPatt {
                path_root: p.path_root,
                tree_opt: p.tree_opt,
            })),
            Expression::Reference(r) => Ok(Pattern::ReferencePatt(ReferencePatt {
                reference_op: r.reference_op,
                pattern: Box::new(Pattern::try_from(*r.expression)?),
            })),
            Expression::Grouped(g) => Ok(Pattern::try_from(*g.inner_expression)?),
            Expression::Range(r) => {
                if r.from_expr_opt.is_none() && r.to_expr_opt.is_none() {
                    if r.range_op == RangeOp::RangeExclusive {
                        Ok(Pattern::RestPatt(RestPatt {
                            dbl_dot: r.range_op,
                        }))
                    } else {
                        Err(ParserErrorKind::UnexpectedRangeOp {
                            expected: format!("`{}`", RangeOp::RangeExclusive),
                            found: format!("`{}`", r.range_op),
                        })
                    }
                } else {
                    Ok(Pattern::RangePatt(RangePatt {
                        from_pattern_opt: {
                            if let Some(e) = r.from_expr_opt {
                                Some(Box::new(Pattern::try_from(Expression::from(*e))?))
                            } else {
                                None
                            }
                        },
                        range_op: r.range_op,
                        to_pattern_opt: {
                            if let Some(e) = r.to_expr_opt {
                                Some(Box::new(Pattern::try_from(Expression::from(*e))?))
                            } else {
                                None
                            }
                        },
                    }))
                }
            }
            Expression::Underscore(u) => Ok(Pattern::WildcardPatt(WildcardPatt {
                underscore: u.underscore,
            })),
            Expression::Tuple(t) => Ok(Pattern::TuplePatt(TuplePatt {
                tuple_patt_elements: {
                    let mut elements: Vec<Pattern> = Vec::new();

                    for te in t.tuple_elements.elements {
                        elements.push(Pattern::try_from(te)?)
                    }

                    let final_element_opt = if let Some(e) = t.tuple_elements.final_element_opt {
                        Some(Box::new(Pattern::try_from(*e)?))
                    } else {
                        None
                    };

                    TuplePattElements {
                        elements,
                        final_element_opt,
                    }
                },
            })),
            Expression::Struct(s) => Ok(Pattern::StructPatt(StructPatt {
                struct_path: PathPatt {
                    path_root: s.struct_path.path_root,
                    tree_opt: s.struct_path.tree_opt,
                },
                struct_fields_opt: {
                    if let Some(sf) = s.struct_fields_opt {
                        let mut struct_patt_fields: Vec<StructPattField> = Vec::new();
                        {
                            for f in sf {
                                struct_patt_fields.push(StructPattField {
                                    field_name: f.field_name,
                                    field_value: Pattern::try_from(*f.field_value)?,
                                })
                            }
                        };

                        Some(struct_patt_fields)
                    } else {
                        None
                    }
                },
            })),
            Expression::SomeExpr(s) => Ok(Pattern::SomePatt(SomePatt {
                kw_some: s.kw_some,
                pattern: {
                    let expr = Expression::Grouped(*s.expression);
                    let patt = Pattern::try_from(expr)?;

                    Box::new(GroupedPatt {
                        inner_pattern: Box::new(patt),
                    })
                },
            })),
            Expression::NoneExpr(n) => Ok(Pattern::NonePatt(NonePatt { kw_none: n.kw_none })),
            Expression::ResultExpr(r) => Ok(Pattern::ResultPatt(ResultPatt {
                kw_ok_or_err: r.kw_ok_or_err,
                pattern: {
                    let expr = Expression::Grouped(*r.expression);
                    let patt = Pattern::try_from(expr)?;

                    Box::new(GroupedPatt {
                        inner_pattern: Box::new(patt),
                    })
                },
            })),
            _ => Err(ParserErrorKind::UnexpectedExpression {
                expected: "pattern-like expression".to_string(),
                found: format!("{}", value),
            }),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            Pattern::Literal(l) => write!(f, "{}", Expression::Literal(l)),
            Pattern::IdentifierPatt(id) => write!(
                f,
                "{}{}{}",
                {
                    if let Some(r) = id.kw_ref_opt {
                        format!("{} ", r)
                    } else {
                        "".to_string()
                    }
                },
                {
                    if let Some(m) = id.kw_mut_opt {
                        format!("{} ", m)
                    } else {
                        "".to_string()
                    }
                },
                id.name
            ),
            Pattern::PathPatt(pth) => write!(f, "{}", PathType::from(pth.clone()).to_string()),
            Pattern::ReferencePatt(r) => write!(f, "{:?}", r),
            Pattern::GroupedPatt(g) => write!(f, "({})", *g.inner_pattern),
            Pattern::RangePatt(rng) => write!(
                f,
                "{}{}{}",
                rng.from_pattern_opt
                    .unwrap_or(Box::new(Pattern::Literal(Literal::Int {
                        value: Int::I64(i64::MIN),
                        span: Span::new("", 0, 0)
                    }))),
                rng.range_op,
                rng.to_pattern_opt
                    .unwrap_or(Box::new(Pattern::Literal(Literal::BigUInt {
                        value: BigUInt::U512(U512::MAX),
                        span: Span::new("", 0, 0)
                    })))
            ),
            Pattern::TuplePatt(tup) => write!(f, "( {} )", tup.tuple_patt_elements),
            Pattern::StructPatt(strc) => {
                write!(
                    f,
                    "{} {{ {:#?} }}",
                    strc.struct_path, strc.struct_fields_opt
                )
            }
            Pattern::TupleStructPatt(ts) => {
                write!(f, "{} ( {:#?} )", ts.struct_path, ts.struct_elements_opt)
            }
            Pattern::WildcardPatt(_) => write!(f, "*"),
            Pattern::RestPatt(_) => write!(f, ".."),
            Pattern::SomePatt(som) => write!(f, "Some{}", Pattern::GroupedPatt(*som.pattern)),
            Pattern::NonePatt(_) => write!(f, "None"),
            Pattern::ResultPatt(res) => write!(f, "{}", {
                match res.kw_ok_or_err {
                    Keyword::Ok => format!("Ok{}", Pattern::GroupedPatt(*res.pattern)),
                    Keyword::Err => format!("Err{}", Pattern::GroupedPatt(*res.pattern)),
                    _ => "".to_string(),
                }
            }),
        }
    }
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

impl Spanned for Statement {
    fn span(&self) -> Span {
        match self.clone() {
            Statement::Let(l) => l.span,
            Statement::Item(i) => match i {
                Item::ImportDecl(id) => id.span,
                Item::AliasDecl(ad) => ad.span,
                Item::ConstantDecl(cd) => cd.span,
                Item::StaticVarDecl(svd) => svd.span,
                Item::ModuleItem(mi) => mi.span,
                Item::TraitDef(td) => td.span,
                Item::EnumDef(ed) => ed.span,
                Item::StructDef(sd) => sd.span,
                Item::TupleStructDef(tsd) => tsd.span,
                Item::InherentImplDef(iid) => iid.span,
                Item::TraitImplDef(tid) => tid.span,
                Item::FunctionItem(fi) => fi.span,
            },
            Statement::Expression(e) => e.span(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            Statement::Let(l) => write!(
                f,
                "let {}: {} = {};",
                l.assignee.name,
                l.type_ann_opt
                    .clone()
                    .unwrap_or(Type::InferredType(InferredType {
                        name: Identifier::from("")
                    }))
                    .clone(),
                l.value_opt.unwrap_or(Expression::NoneExpr(NoneExpr {
                    kw_none: Keyword::None,
                    span: l.span
                }))
            ),
            Statement::Item(it) => match it {
                Item::ImportDecl(im) => {
                    write!(f, "{}import {};", im.visibility, im.import_tree)
                }
                Item::AliasDecl(ad) => write!(
                    f,
                    "{}alias {} = {};",
                    ad.visibility,
                    ad.alias_name,
                    ad.original_type_opt.unwrap_or(Type::UnitType(Unit))
                ),
                Item::ConstantDecl(cd) => write!(
                    f,
                    "{}const {}: {} = {:?};",
                    cd.visibility,
                    cd.constant_name,
                    *cd.constant_type,
                    cd.value_opt.unwrap_or(ValueExpr::NoneExpr(NoneExpr {
                        kw_none: Keyword::None,
                        span: cd.span
                    }))
                ),
                Item::StaticVarDecl(svd) => write!(
                    f,
                    "{}static {}: {} = {:?};",
                    svd.visibility,
                    svd.var_name,
                    svd.var_type,
                    svd.assignee_opt
                        .unwrap_or(Box::new(AssigneeExpr::TupleExpr {
                            elements: Vec::new(),
                            span: svd.span
                        }))
                ),
                Item::ModuleItem(m) => {
                    write!(
                        f,
                        "{}module {} {{ #![{:?}] {:?} }}",
                        m.visibility,
                        m.module_name,
                        m.inner_attributes_opt,
                        m.items_opt.unwrap_or(Vec::new())
                    )
                }
                Item::TraitDef(td) => write!(
                    f,
                    "{}trait {} {{ #![{:?}] {:?} }}",
                    td.visibility,
                    td.trait_name,
                    td.inner_attributes_opt,
                    td.trait_items_opt.unwrap_or(Vec::new())
                ),
                Item::EnumDef(ed) => write!(
                    f,
                    "{}enum {} {{ {:?} }}",
                    ed.visibility, ed.enum_name, ed.variants
                ),
                Item::StructDef(sd) => write!(
                    f,
                    "{}struct {} {{ {:?} }}",
                    sd.visibility,
                    sd.struct_name,
                    sd.fields_opt.unwrap_or(Vec::new())
                ),
                Item::TupleStructDef(tsd) => write!(
                    f,
                    "{}struct {} {{ {:?} }}",
                    tsd.visibility,
                    tsd.struct_name,
                    tsd.elements_opt.unwrap_or(Vec::new())
                ),
                Item::InherentImplDef(iid) => write!(
                    f,
                    "impl {} {{ {:?} }}",
                    iid.nominal_type,
                    iid.associated_items_opt.unwrap_or(Vec::new())
                ),
                Item::TraitImplDef(tid) => write!(
                    f,
                    "impl {} for {} {{ {:?} }}",
                    tid.implemented_trait_path,
                    tid.implementing_type,
                    tid.associated_items_opt.unwrap_or(Vec::new())
                ),
                Item::FunctionItem(fi) => write!(
                    f,
                    "{}func {}({:?}) -> {} {{ {:?} }}",
                    fi.visibility,
                    fi.function_name,
                    fi.params_opt,
                    fi.return_type_opt.unwrap_or(Box::new(Type::UnitType(Unit))),
                    fi.block_opt
                ),
            },
            Statement::Expression(e) => write!(f, "{}", e),
        }
    }
}

/// Enum representing the different item nodes in the AST.
/// An item is a component of a library, organized by a set of modules.
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Type {
    // primitives
    I32(Int),
    I64(Int),
    U8(UInt),
    U16(UInt),
    U32(UInt),
    U64(UInt),
    U256(BigUInt),
    U512(BigUInt),
    F32(Float),
    F64(Float),
    Byte(Byte),
    B2(Bytes),
    B4(Bytes),
    B8(Bytes),
    B16(Bytes),
    B32(Bytes),
    H160(types::Hash),
    H256(types::Hash),
    H512(types::Hash),
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
}

impl From<Type> for PathType {
    fn from(value: Type) -> Self {
        match value {
            Type::UserDefined(p) => p,
            t => PathType::from(Identifier::from(&t.to_string())),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32(_) => write!(f, "i32"),
            Type::I64(_) => write!(f, "i64"),
            Type::U8(_) => write!(f, "u8"),
            Type::U16(_) => write!(f, "u16"),
            Type::U32(_) => write!(f, "u32"),
            Type::U64(_) => write!(f, "u64"),
            Type::U256(_) => write!(f, "u256"),
            Type::U512(_) => write!(f, "u512"),
            Type::Byte(_) => write!(f, "byte"),
            Type::F32(_) => write!(f, "f32"),
            Type::F64(_) => write!(f, "f64"),
            Type::B2(_) => write!(f, "b2"),
            Type::B4(_) => write!(f, "b4"),
            Type::B8(_) => write!(f, "b8"),
            Type::B16(_) => write!(f, "b16"),
            Type::B32(_) => write!(f, "b32"),
            Type::H160(_) => write!(f, "h160"),
            Type::H256(_) => write!(f, "h256"),
            Type::H512(_) => write!(f, "h512"),
            Type::Str(_) => write!(f, "str"),
            Type::Char(_) => write!(f, "char"),
            Type::Bool(_) => write!(f, "bool"),
            Type::UnitType(_) => write!(f, "()"),
            Type::GroupedType(g) => write!(f, "({})", *g),
            Type::Array {
                element_type,
                num_elements,
            } => write!(f, "[{}; {}]", *element_type, num_elements),
            Type::Tuple(t) => write!(f, "({:?})", t),
            Type::UserDefined(ud) => write!(f, "{}", ud),
            Type::FunctionPtr(fp) => write!(f, "{}", fp),
            Type::Reference {
                reference_op,
                inner_type,
            } => match reference_op {
                ReferenceOp::Borrow => write!(f, "{}{}", reference_op, *inner_type),
                ReferenceOp::MutableBorrow => write!(f, "{} {}", reference_op, *inner_type),
            },
            Type::SelfType(_) => write!(f, "Self"),
            Type::InferredType(_) => write!(f, "_"),
            Type::Vec { element_type } => write!(f, "Vec<{}>", *element_type),
            Type::Mapping {
                key_type,
                value_type,
            } => write!(f, "Mapping<{}, {}>", *key_type, *value_type),
            Type::Option { inner_type } => write!(f, "Option<{}>", *inner_type),
            Type::Result { ok_type, err_type } => {
                write!(f, "Result<{}, {}>", *ok_type, *err_type)
            }
        }
    }
}
