use super::{
    AssigneeExpr, AssignmentOp, BinaryOp, ComparisonOp, CompoundAssignmentOp, Delimiter,
    DereferenceOp, Expression, Identifier, InnerAttr, Keyword, OuterAttr, Pattern, RangeOp,
    ReferenceOp, SelfType, Separator, Statement, Type, UInt, UnaryOp, UnwrapOp, ValueExpr,
};

///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Enum representing whether or not a closure has parameters in its definition.
#[derive(Debug, Clone, PartialEq)]
pub enum ClosureParams {
    Some(Vec<ClosureParam>), // `| params |`
    None,                    // `||`
}

/// Enum representing the different path root options.
#[derive(Debug, Clone, PartialEq)]
pub enum PathPrefix {
    Package,
    Super,
    SelfKeyword,
    SelfType(SelfType),
    Identifier(Identifier),
}

/// Struct representing a closure parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct ClosureParam {
    pub param_name: Pattern,
    pub type_ann_opt: Option<Type>,
}

/// Struct representing a single arm in a match statement.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard_opt: Option<(Keyword, Box<Expression>)>, // `if (..)`
    pub body: Box<Expression>,
}

/// Struct representing a single field in a struct expression, with a name, value
/// and optional attributes.
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub field_name: Identifier,
    pub field_value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructAssigneeExprField {
    pub attributes_opt: Option<Vec<OuterAttr>>,
    pub field_name: Identifier,
    pub field_value: AssigneeExpr,
}

/// Struct representing a collection of elements in a tuple expression.
#[derive(Debug, Clone, PartialEq)]
pub struct TupleElements {
    pub elements: Vec<(Expression, Separator)>, // single-element tuple must have trailing comma
    pub final_element_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleAssigneeExprElements {
    pub elements: Vec<(AssigneeExpr, Separator)>,
    pub final_element_opt: Option<Box<AssigneeExpr>>,
}

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub open_bracket: Delimiter,
    pub elements_opt: Option<Vec<Expression>>,
    pub close_bracket: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpr {
    pub lhs: AssigneeExpr,
    pub assignment_op: AssignmentOp,
    pub rhs: ValueExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Box<ValueExpr>,
    pub binary_op: BinaryOp,
    pub rhs: Box<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonExpr {
    pub lhs: AssigneeExpr,
    pub comparison_op: ComparisonOp,
    pub rhs: AssigneeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub attributes_opt: Option<Vec<InnerAttr>>,
    pub open_brace: Delimiter,
    pub statements_opt: Option<Vec<Statement>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceExpr {
    pub reference_op: ReferenceOp,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakExpr {
    pub kw_break: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: AssigneeExpr,
    pub open_paren: Delimiter,
    pub args_opt: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureExpr {
    pub params: ClosureParams,
    pub return_type_opt: Option<Box<Type>>,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundAssignmentExpr {
    pub lhs: AssigneeExpr,
    pub compound_assignment_op: CompoundAssignmentOp,
    pub rhs: ValueExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueExpr {
    pub kw_continue: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DereferenceExpr {
    pub dereference_op: DereferenceOp,
    pub assignee_expr: AssigneeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccessExpr {
    pub object: Box<AssigneeExpr>,
    pub field: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForInExpr {
    pub kw_for: Keyword,
    pub assignee: Pattern,
    pub kw_in: Keyword,
    pub iterable: Box<Expression>,
    pub block: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedExpr {
    pub open_paren: Delimiter,
    pub expression: Box<Expression>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub kw_if: Keyword,
    pub condition: Box<Expression>,
    pub if_block: Box<Expression>,
    pub else_if_blocks_opt: Option<Vec<(Keyword, Box<Expression>)>>, // `else`, `if { .. }`
    pub trailing_else_block_opt: Option<(Keyword, Box<Expression>)>, // `else { .. }`
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub array: Box<AssigneeExpr>,
    pub open_bracket: Delimiter,
    pub index: Box<Expression>,
    pub close_bracket: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    pub kw_match: Keyword,
    pub scrutinee: AssigneeExpr,
    pub open_brace: Delimiter,
    pub arms_opt: Option<Vec<MatchArm>>,
    pub final_arm: Box<MatchArm>, // default case
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodCallExpr {
    pub receiver: Box<AssigneeExpr>,
    pub method_name: Identifier,
    pub open_paren: Delimiter,
    pub args_opt: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NoneExpr {
    pub kw_none: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathExpr {
    pub root: PathPrefix,
    pub tree_opt: Option<Vec<Identifier>>,
    pub wildcard_opt: Option<Separator>, // `::*`
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpr {
    pub from_opt: Option<Box<Expression>>,
    pub range_op: RangeOp, // `..` or `..=`
    pub to_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResultExpr {
    pub kw_ok_or_err: Keyword,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnExpr {
    pub kw_return: Keyword,
    pub expression_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SomeExpr {
    pub kw_some: Keyword,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructExpr {
    pub path: PathExpr,
    pub open_brace: Delimiter,
    pub fields_opt: Option<Vec<StructField>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub open_paren: Delimiter,
    pub tuple_elements: TupleElements,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleIndexExpr {
    pub operand: Box<AssigneeExpr>,
    pub index: UInt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructExpr {
    pub path: PathExpr,
    pub open_paren: Delimiter,
    pub elements_opt: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCastExpr {
    pub operand: Box<ValueExpr>,
    pub kw_as: Keyword,
    pub new_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnderscoreExpr {
    pub underscore: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub unary_op: UnaryOp,
    pub value_expr: Box<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnwrapExpr {
    pub value_expr: Box<ValueExpr>,
    pub op: UnwrapOp,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {
    pub kw_while: Keyword,
    pub condition: Box<Expression>,
    pub block: Box<Expression>,
}
