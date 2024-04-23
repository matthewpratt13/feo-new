use super::{
    AssigneeExpr, AssignmentOp, BinaryOp, CompoundAssignmentOp, Delimiter, Expression, Identifier,
    Keyword, RangeOp, Separator, Statement, Type, UInt, UnaryOp, UnwrapOp, ValueExpr,
};

///////////////////////////////////////////////////////////////////////////
/// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Enum representing whether or not a closure has parameters in its definition.
#[derive(Debug, Clone)]
pub enum ClosureParams {
    Some(Separator, Vec<ClosureParam>, Separator),
    None(Separator),
}

/// Enum representing the different path root options.
#[derive(Debug, Clone)]
pub enum PathPrefix {
    Package,
    Super,
    SelfKeyword,
    SelfType,
    Identifier(Identifier),
}

/// Struct representing a closure parameter.
#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub id: Identifier,
    pub ty: Option<Type>,
}

/// Struct representing a single field in a struct expression, with a name and value.
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct TupleElements {
    pub elements: Vec<(Expression, Separator)>, // single-element tuple must have trailing comma
    pub final_element_opt: Option<Box<Expression>>,
}

/// Struct representing a single arm in a match statement.
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub case: Box<Expression>,
    pub guard_opt: Option<(Keyword, Box<GroupedExpr>)>, // `if (..)`
    pub fat_arrow: Separator,
    pub logic: Box<Expression>,
}

///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct ArrayExpr {
    pub open_bracket: Delimiter,
    pub elements_opt: Option<Vec<Expression>>,
    pub close_bracket: Delimiter,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub lhs: AssigneeExpr,
    pub op: AssignmentOp,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub lhs: Box<Expression>, // `ValueExpr`?
    pub op: BinaryOp,
    pub rhs: Box<Expression>, // `ValueExpr`?
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub kw_unsafe_opt: Option<Keyword>,
    pub open_brace: Delimiter,
    pub statements_opt: Option<Vec<Statement>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct BorrowExpr {
    pub op: UnaryOp,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BreakExpr {
    pub kw_break: Keyword,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: AssigneeExpr,
    pub open_paren: Delimiter,
    pub args_opt: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct ClosureExpr {
    pub params: ClosureParams,
    pub return_type_opt: Option<(Separator, Type)>, // `-> Type`
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct CompoundAssignmentExpr {
    pub lhs: AssigneeExpr,
    pub op: CompoundAssignmentOp,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ContinueExpr {
    pub kw_continue: Keyword,
}

#[derive(Debug, Clone)]
pub struct DereferenceExpr {
    pub op: UnaryOp,
    pub expression: AssigneeExpr,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    pub object: Box<AssigneeExpr>,
    pub dot: Separator,
    pub field: Identifier,
}

#[derive(Debug, Clone)]
pub struct ForInExpr {
    pub kw_for: Keyword,
    pub assignee: Box<Expression>,
    pub kw_in: Keyword,
    pub iterable: Box<Expression>,
    pub block: BlockExpr,
}

#[derive(Debug, Clone)]
pub struct GroupedExpr {
    pub open_paren: Delimiter,
    pub expression: Box<Expression>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub kw_if: Keyword,
    pub condition: Box<GroupedExpr>,
    pub if_block: Box<BlockExpr>,
    pub else_if_blocks_opt: Option<Vec<(Keyword, Box<IfExpr>)>>, // `else`, `if { .. }`
    pub trailing_else_block_opt: Option<(Keyword, BlockExpr)>,   // `else { .. }`
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub array: Box<AssigneeExpr>,
    pub open_bracket: Delimiter,
    pub index: Box<Expression>,
    pub close_bracket: Delimiter,
}

#[derive(Debug, Clone)]
pub struct MatchExpr {
    pub kw_match: Keyword,
    pub scrutinee: AssigneeExpr,
    pub open_brace: Delimiter,
    pub arms_opt: Option<Vec<MatchArm>>,
    pub final_arm: MatchArm, // default case
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub receiver: AssigneeExpr,
    pub dot: Separator,
    pub method_name: Identifier,
    pub open_paren: Delimiter,
    pub args_opt: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct NegationExpr {
    pub op: UnaryOp,
    pub expression: Box<ValueExpr>,
}

#[derive(Debug, Clone)]
pub struct NoneExpr {
    pub kw_none: Keyword,
}

#[derive(Debug, Clone)]
pub struct PathExpr {
    pub root: PathPrefix,
    pub tree_opt: Option<Vec<Identifier>>,
    pub wildcard_opt: Option<Separator>, // `::*`
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub from_opt: Option<Box<Expression>>,
    pub op: RangeOp, // `..` or `..=`
    pub to_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct ResultExpr {
    pub kw_ok_or_err: Keyword,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ReturnExpr {
    pub kw_return: Keyword,
    pub expression_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct SomeExpr {
    pub kw_some: Keyword,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct StructExpr {
    pub path: PathExpr,
    pub open_brace: Delimiter,
    pub fields_opt: Option<Vec<StructField>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct TupleExpr {
    pub open_paren: Delimiter,
    pub elements_opt: Option<TupleElements>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct TupleIndexExpr {
    pub operand: Box<AssigneeExpr>,
    pub dot: Separator,
    pub index: UInt,
}

#[derive(Debug, Clone)]
pub struct TupleStructExpr {
    pub path: PathExpr,
    pub open_paren: Delimiter,
    pub elements_opt: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct TypeCastExpr {
    pub operand: Box<Expression>,
    pub kw_as: Keyword,
    pub new_type: Type,
}

#[derive(Debug, Clone)]
pub struct UnderscoreExpr {
    pub underscore: Separator,
}

#[derive(Debug, Clone)]
pub struct UnwrapExpr {
    pub expression: Box<ValueExpr>,
    pub op: UnwrapOp,
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
    pub kw_while: Keyword,
    pub condition: Box<GroupedExpr>,
    pub block: BlockExpr,
}
