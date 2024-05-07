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
pub(crate) enum ClosureParams {
    Some(Vec<ClosureParam>), // `| params |`
    None,                    // `||`
}

/// Enum representing the different path root options.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum PathPrefix {
    Package,
    Super,
    SelfKeyword,
    SelfType(SelfType),
    Identifier(Identifier),
}

/// Struct representing a closure parameter.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ClosureParam {
    pub(crate) param_name: Pattern,
    pub(crate) type_ann_opt: Option<Type>,
}

/// Struct representing a key-value pair in a mapping.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MappingPair {
    pub(crate) key: Pattern,
    pub(crate) value: Expression,
}

/// Struct representing a single arm in a match statement.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MatchArm {
    pub(crate) pattern: Pattern,
    pub(crate) guard_opt: Option<(Keyword, Box<Expression>)>, // `if (..)`
    pub(crate) body: Box<Expression>,
}

/// Struct representing a single field in a struct expression, with a name, value
/// and optional attributes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StructField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) field_name: Identifier,
    pub(crate) field_value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StructAssigneeExprField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) field_name: Identifier,
    pub(crate) field_value: AssigneeExpr,
}

/// Struct representing a collection of elements in a tuple expression.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TupleElements {
    pub(crate) elements: Vec<(Expression, Separator)>, // single-element tuple must have trailing comma
    pub(crate) final_element_opt: Option<Box<Expression>>,
}

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub(crate) open_bracket: Delimiter,
    pub(crate) elements_opt: Option<Vec<Expression>>,
    pub(crate) close_bracket: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) assignment_op: AssignmentOp,
    pub(crate) rhs: ValueExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub(crate) lhs: Box<ValueExpr>,
    pub(crate) binary_op: BinaryOp,
    pub(crate) rhs: Box<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) comparison_op: ComparisonOp,
    pub(crate) rhs: AssigneeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub(crate) attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) open_brace: Delimiter,
    pub(crate) statements_opt: Option<Vec<Statement>>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceExpr {
    pub(crate) reference_op: ReferenceOp,
    pub(crate) expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakExpr {
    pub(crate) kw_break: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub(crate) callee: AssigneeExpr,
    pub(crate) open_paren: Delimiter,
    pub(crate) args_opt: Option<Vec<Expression>>,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureExpr {
    pub(crate) params: ClosureParams,
    pub(crate) return_type_opt: Option<Box<Type>>,
    pub(crate) expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundAssignmentExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) compound_assignment_op: CompoundAssignmentOp,
    pub(crate) rhs: ValueExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueExpr {
    pub(crate) kw_continue: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DereferenceExpr {
    pub(crate) dereference_op: DereferenceOp,
    pub(crate) assignee_expr: AssigneeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccessExpr {
    pub(crate) object: Box<AssigneeExpr>,
    pub(crate) field: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForInExpr {
    pub(crate) kw_for: Keyword,
    pub(crate) pattern: Pattern,
    pub(crate) kw_in: Keyword,
    pub(crate) iterable: Box<Expression>,
    pub(crate) block: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedExpr {
    pub(crate) open_paren: Delimiter,
    pub(crate) expression: Box<Expression>,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub(crate) kw_if: Keyword,
    pub(crate) condition: Box<Expression>,
    pub(crate) if_block: Box<Expression>,
    pub(crate) else_if_blocks_opt: Option<Vec<(Keyword, Box<Expression>)>>, // `else`, `if { .. }`
    pub(crate) trailing_else_block_opt: Option<(Keyword, Box<Expression>)>, // `else { .. }`
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub(crate) array: Box<AssigneeExpr>,
    pub(crate) open_bracket: Delimiter,
    pub(crate) index: Box<Expression>,
    pub(crate) close_bracket: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MappingExpr {
    pub(crate) open_brace: Delimiter,
    pub(crate) pairs_opt: Option<Vec<MappingPair>>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    pub(crate) kw_match: Keyword,
    pub(crate) scrutinee: AssigneeExpr,
    pub(crate) open_brace: Delimiter,
    pub(crate) arms_opt: Option<Vec<MatchArm>>,
    pub(crate) final_arm: Box<MatchArm>, // default case
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodCallExpr {
    pub(crate) receiver: Box<AssigneeExpr>,
    pub(crate) method_name: Identifier,
    pub(crate) open_paren: Delimiter,
    pub(crate) args_opt: Option<Vec<Expression>>,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NoneExpr {
    pub(crate) kw_none: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathExpr {
    pub(crate) root: PathPrefix,
    pub(crate) tree_opt: Option<Vec<Identifier>>,
    pub(crate) wildcard_opt: Option<Separator>, // `::*`
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpr {
    pub(crate) from_opt: Option<Box<Expression>>, // TODO: change to `AssigneeExpr`
    pub(crate) range_op: RangeOp, // `..` or `..=`
    pub(crate) to_opt: Option<Box<Expression>>, // TODO: change to `AssigneeExpr`
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResultExpr {
    pub(crate) kw_ok_or_err: Keyword,
    pub(crate) expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnExpr {
    pub(crate) kw_return: Keyword,
    pub(crate) expression_opt: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SomeExpr {
    pub(crate) kw_some: Keyword,
    pub(crate) expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructExpr {
    pub(crate) path: PathExpr,
    pub(crate) open_brace: Delimiter,
    pub(crate) fields_opt: Option<Vec<StructField>>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub(crate) open_paren: Delimiter,
    pub(crate) tuple_elements: TupleElements,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleIndexExpr {
    pub(crate) operand: Box<AssigneeExpr>,
    pub(crate) index: UInt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructExpr {
    pub(crate) path: PathExpr,
    pub(crate) open_paren: Delimiter,
    pub(crate) elements_opt: Option<Vec<Expression>>,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCastExpr {
    pub(crate) operand: Box<ValueExpr>,
    pub(crate) kw_as: Keyword,
    pub(crate) new_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnderscoreExpr {
    pub(crate) underscore: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub(crate) unary_op: UnaryOp,
    pub(crate) value_expr: Box<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnwrapExpr {
    pub(crate) value_expr: Box<ValueExpr>,
    pub(crate) op: UnwrapOp,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {
    pub(crate) kw_while: Keyword,
    pub(crate) condition: Box<Expression>,
    pub(crate) block: Box<Expression>,
}
