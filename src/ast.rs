//! # AST
//!
//! Contains the different nodes that make up the abstract syntax tree (AST) and the tokens
//! within those nodes. The primary nodes are `Expression`, `Item` and `Statement`.

mod expression;
mod item;
mod pattern;
mod statement;
mod types;

use core::fmt;

use crate::{error::ParserErrorKind, span::Position};

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
    Char(Char),
    Bool(Bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::UInt(ui) => write!(f, "{}", ui),
            Literal::BigUInt(bui) => write!(f, "{}", bui),
            Literal::Byte(by) => write!(f, "{}", by),
            Literal::Bytes(bb) => write!(f, "{}", bb),
            Literal::Hash(h) => write!(f, "{}", h),
            Literal::Str(s) => write!(f, "{}", s),
            Literal::Char(c) => write!(f, "{}", c),
            Literal::Bool(b) => write!(f, "{}", b),
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// IDENTIFIER
///////////////////////////////////////////////////////////////////////////

/// Wrapper type, turning a `String` into an `Identifier`.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Identifier(pub String);

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl From<&String> for Identifier {
    fn from(value: &String) -> Self {
        Self(value.to_string())
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
            Keyword::Package => write!(f, "package"),
            Keyword::SelfKeyword => write!(f, "self"),
            Keyword::Pub => write!(f, "pub"),
            Keyword::As => write!(f, "as"),
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
    LParen { position: Position },
    // RParen { position: Position },
    LBracket { position: Position },
    // RBracket { position: Position },
    LBrace { position: Position },
    // RBrace { position: Position },
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
            // Delimiter::RParen { .. } => write!(f, ")"),
            Delimiter::LBracket { .. } => write!(f, "["),
            // Delimiter::RBracket { .. } => write!(f, "]"),
            Delimiter::LBrace { .. } => write!(f, "{{"),
            // Delimiter::RBrace { .. } => write!(f, "}}"),
            Delimiter::Pipe { .. } => write!(f, "|"),
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// PUNCTUATION
///////////////////////////////////////////////////////////////////////////

/// Unit struct representing the assignment operator (`=`) used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AssignmentOp;

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

/// Unit struct representing the type cast operator (`as`).
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TypeCastOp;

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

/// Unit struct representing the dereference operator (`*`).
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct DereferenceOp;

/// Enum representing the different range operators used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RangeOp {
    RangeExclusive, // `..`
    RangeInclusive, // `..=`
}

impl fmt::Display for RangeOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RangeOp::RangeExclusive => write!(f, "`..`"),
            RangeOp::RangeInclusive => write!(f, "`..=`"),
        }
    }
}

/// Enum representing the different reference operators used in AST nodes (i.e., `&` and `&mut`).
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Separator {
    Comma,              // used in tuples
    ColonColonAsterisk, // path wildcard terminator
}

/// Enum representing the different unary operators used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnaryOp {
    Negate, // `-`
    Not,    // `!`
}

/// Unit struct representing the unwrap operator (`?`) used in AST nodes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UnwrapOp;

///////////////////////////////////////////////////////////////////////////
// NODE GROUPS
///////////////////////////////////////////////////////////////////////////

/// Enum representing the different types of expression in the AST.
/// `Expression` nodes always produce or evaluate to a value and may have *side effects*.
/// Expressions can function differently in various contexts; i.e., they can act both as values
/// and as locations in memory. This distinction refers to **value expressions**
/// and **place expressions**. See additional expression type enumerations below for both cases.
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal(l) => match l {
                Literal::Int(i) => write!(f, "{}", i),
                Literal::UInt(ui) => write!(f, "{}", ui),
                Literal::BigUInt(bui) => write!(f, "{}", bui),
                Literal::Byte(by) => write!(f, "{}", by),
                Literal::Bytes(bb) => write!(f, "{}", bb),
                Literal::Hash(h) => write!(f, "{}", h),
                Literal::Str(s) => write!(f, "{}", s),
                Literal::Char(c) => write!(f, "{}", c),
                Literal::Bool(b) => write!(f, "{}", b),
            },
            Expression::Path(pth) => write!(f, "{:?}", pth),
            Expression::MethodCall(mc) => write!(f, "{:?}", mc),
            Expression::FieldAccess(fa) => write!(f, "{:?}", fa),
            Expression::Call(cal) => write!(f, "{:?}", cal),
            Expression::Index(i) => write!(f, "{:?}", i),
            Expression::TupleIndex(ti) => write!(f, "{:?}", ti),
            Expression::Unwrap(unw) => write!(f, "{:?}", unw),
            Expression::Unary(una) => write!(f, "{:?}", una),
            Expression::Reference(r) => write!(f, "{:?}", r),
            Expression::Dereference(dr) => write!(f, "{:?}", dr),
            Expression::TypeCast(tc) => write!(f, "{:?}", tc),
            Expression::Binary(bin) => write!(f, "{:?}", bin),
            Expression::Comparison(cmp) => write!(f, "{:?}", cmp),
            Expression::Grouped(grp) => write!(f, "({})", *grp.inner_expression),
            Expression::Range(rng) => write!(f, "{:?}", rng),
            Expression::Assignment(asn) => write!(f, "{:?}", asn),
            Expression::CompoundAssignment(casn) => write!(f, "{:?}", casn),
            Expression::Return(ret) => write!(f, "return  {:?}", ret.expression_opt),
            Expression::Break(_) => write!(f, "break"),
            Expression::Continue(_) => write!(f, "continue"),
            Expression::Underscore(_) => write!(f, "_"),
            Expression::Closure(clo) => write!(f, "{:?}", clo),
            Expression::Array(arr) => write!(f, "{:?}", arr),
            Expression::Tuple(tup) => write!(f, "{:?}", tup),
            Expression::Struct(strc) => write!(f, "{:?}", strc),
            Expression::Mapping(map) => write!(f, "{:?}", map),
            Expression::Block(blk) => write!(f, "{:?}", blk),
            Expression::If(ifex) => write!(f, "{:?}", ifex),
            Expression::Match(mat) => write!(f, "{:?}", mat),
            Expression::ForIn(fi) => write!(f, "{:?}", fi),
            Expression::While(whl) => write!(f, "{:?}", whl),
            Expression::SomeExpr(som) => write!(f, "{:?}", som),
            Expression::NoneExpr(_) => write!(f, "None"),
            Expression::ResultExpr(res) => write!(f, "{:?}", res),
        }
    }
}

/// Enum representing expressions behaving in a **value expression** context.
/// A **value expression** is an expression that represents an actual value, as opposed
/// to a **place expression**, which represents a location in memory.
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

/// Enum representing expressions behaving in a place / assignee expression context.
/// **Assignee expressions** usually occur on the left hand side of assignment expressions,
/// and cover all **place expressions**, the underscore expression, plus arrays
/// of assignee expressions, tuples of assignee expressions, and structs of assignee expressions.
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
    ArrayExpr(Vec<AssigneeExpr>),
    TupleExpr(Vec<AssigneeExpr>),
    StructExpr(Vec<StructAssigneeExprField>),
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
                let assignee_expression = AssigneeExpr::try_from(*g.inner_expression)?;
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

                Ok(AssigneeExpr::ArrayExpr(assignee_expressions))
            }

            Expression::Tuple(t) => {
                let assignee_expressions = t
                    .tuple_elements
                    .elements
                    .into_iter()
                    .map(|te| {
                        AssigneeExpr::try_from(te).expect(
                            "conversion error: unable to convert `Expression` into `AssigneeExpr`",
                        )
                    })
                    .collect::<Vec<AssigneeExpr>>();

                Ok(AssigneeExpr::TupleExpr(assignee_expressions))
            }

            Expression::Struct(s) => {
                let mut assignee_expressions: Vec<StructAssigneeExprField> = Vec::new();

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
                        assignee_expressions.push(struct_assignee_expr_field);
                    })
                });

                Ok(AssigneeExpr::StructExpr(assignee_expressions))
            }

            _ => Err(ParserErrorKind::UnexpectedExpression {
                expected: "assignee expression".to_string(),
                found: format!("{}", value),
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
    TupleStructPatt(TupleStructPatt),
    WildcardPatt(WildcardPatt),
    RestPatt(RestPatt),
    SomePatt(SomePatt),
    NonePatt(NonePatt),
    ResultPatt(ResultPatt),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Literal(l) => match l {
                Literal::Int(i) => write!(f, "{}", i),
                Literal::UInt(ui) => write!(f, "{}", ui),
                Literal::BigUInt(bui) => write!(f, "{}", bui),
                Literal::Byte(by) => write!(f, "{}", by),
                Literal::Bytes(bb) => write!(f, "{}", bb),
                Literal::Hash(h) => write!(f, "{}", h),
                Literal::Str(s) => write!(f, "{}", s),
                Literal::Char(c) => write!(f, "{}", c),
                Literal::Bool(b) => write!(f, "{}", b),
            },
            Pattern::IdentifierPatt(id) => write!(f, "{}", id.name),
            Pattern::PathPatt(pth) => write!(f, "{:?}", pth),
            Pattern::ReferencePatt(r) => write!(f, "{:?}", r),
            Pattern::GroupedPatt(g) => write!(f, "({})", *g.inner_pattern),
            Pattern::RangePatt(rng) => write!(f, "{:?}", rng),
            Pattern::TuplePatt(tup) => write!(f, "{:?}", tup),
            Pattern::StructPatt(s) => write!(f, "{:?}", s),
            Pattern::TupleStructPatt(ts) => write!(f, "{:?}", ts),
            Pattern::WildcardPatt(_) => write!(f, "*"),
            Pattern::RestPatt(_) => write!(f, ".."),
            Pattern::SomePatt(som) => write!(f, "{:?}", som),
            Pattern::NonePatt(_) => write!(f, "None"),
            Pattern::ResultPatt(res) => write!(f, "{:?}", res),
        }
    }
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
    StaticVarDecl(StaticVarDecl),
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
        ok_type: Box<Type>,
        err_type: Box<Type>,
    },
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32(_) => write!(f, "i32"),
            Type::I64(_) => write!(f, "i64"),
            Type::I128(_) => write!(f, "i128"),
            Type::U8(_) => write!(f, "u8"),
            Type::U16(_) => write!(f, "u16"),
            Type::U32(_) => write!(f, "u32"),
            Type::U64(_) => write!(f, "u64"),
            Type::U128(_) => write!(f, "u128"),
            Type::U256(_) => write!(f, "u256"),
            Type::U512(_) => write!(f, "u512"),
            Type::Byte(_) => write!(f, "byte"),
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
            Type::UserDefined(ud) => write!(f, "{:?}", ud),
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
            Type::Vec(v) => write!(f, "Vec<{}>", *v),
            Type::Mapping {
                key_type,
                value_type,
            } => write!(f, "Mapping<{}, {}>", *key_type, *value_type),
            Type::Option { inner_type } => write!(f, "Option<{}>", *inner_type),
            Type::Result { ok_type, err_type } => write!(f, "Result<{}, {}>", *ok_type, *err_type),
        }
    }
}
