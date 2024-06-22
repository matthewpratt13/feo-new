use core::fmt;

use crate::span::Span;

use super::{
    AssigneeExpr, AssignmentOp, BinaryOp, ComparisonOp, CompoundAssignmentOp, DereferenceOp,
    Expression, Identifier, IdentifierPatt, InnerAttr, Keyword, OuterAttr, Pattern, RangeOp,
    ReferenceOp, SelfType, Statement, Type, TypeCastOp, UInt, UnaryOp, UnwrapOp, ValueExpr,
};

///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Enum representing whether or not a closure has parameters in its definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ClosureParams {
    Some(Vec<ClosureParam>), // `| <param>: <Type>, .. |`
    None,                    // `||`
}

/// Enum representing the different path root options.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum PathRoot {
    Package,
    Super,
    SelfKeyword,
    SelfType(SelfType),
    Identifier(Identifier),
}

impl fmt::Display for PathRoot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathRoot::Package => write!(f, "package"),
            PathRoot::Super => write!(f, "super"),
            PathRoot::SelfKeyword => write!(f, "self"),
            PathRoot::SelfType(_) => write!(f, "Self"),
            PathRoot::Identifier(i) => write!(f, "{}", i.to_string()),
        }
    }
}

/// Struct representing a closure parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ClosureParam {
    pub(crate) param_name: IdentifierPatt,
    pub(crate) type_ann_opt: Option<Box<Type>>,
}

/// Struct representing a key-value pair in a mapping.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MappingPair {
    pub(crate) key: IdentifierPatt,
    pub(crate) value: Box<Expression>,
}

/// Struct representing a single arm in a match statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MatchArm {
    pub(crate) matched_pattern: Pattern,
    pub(crate) guard_opt: Option<Box<Expression>>, // `<case> if <expr>`
    pub(crate) arm_expression: Box<Expression>,
}

/// Struct representing a single field in a struct assignee expression, with a name, assignee
/// and optional attributes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructAssigneeExprField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) field_name: Identifier,
    pub(crate) field_value: Box<AssigneeExpr>,
}

/// Struct representing a single field in a struct expression, with a name, value
/// and optional attributes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) field_name: Identifier,
    pub(crate) field_value: Box<Expression>,
}

/// Struct representing a collection of elements in a tuple expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TupleElements {
    pub(crate) elements: Vec<Expression>,
    pub(crate) final_element_opt: Option<Box<Expression>>,
}

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayExpr {
    pub(crate) elements_opt: Option<Vec<Expression>>, // arrays can be empty, hence optional
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignmentExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) assignment_op: AssignmentOp,
    pub(crate) rhs: ValueExpr,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub(crate) lhs: Box<ValueExpr>,
    pub(crate) binary_op: BinaryOp,
    pub(crate) rhs: Box<ValueExpr>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockExpr {
    pub(crate) attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) statements_opt: Option<Vec<Statement>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BreakExpr {
    pub(crate) kw_break: Keyword,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpr {
    pub(crate) callee: AssigneeExpr, // function being called
    pub(crate) args_opt: Option<Vec<Expression>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosureExpr {
    pub(crate) closure_params: ClosureParams,
    pub(crate) return_type_opt: Option<Box<Type>>,
    pub(crate) body_expression: Box<Expression>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ComparisonExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) comparison_op: ComparisonOp,
    pub(crate) rhs: AssigneeExpr,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundAssignmentExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) compound_assignment_op: CompoundAssignmentOp,
    pub(crate) rhs: ValueExpr,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContinueExpr {
    pub(crate) kw_continue: Keyword,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DereferenceExpr {
    pub(crate) dereference_op: DereferenceOp,
    pub(crate) assignee_expr: AssigneeExpr,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccessExpr {
    pub(crate) object: Box<AssigneeExpr>,
    pub(crate) field_name: Identifier,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForInExpr {
    pub(crate) kw_for: Keyword,
    pub(crate) pattern: Box<Pattern>,
    pub(crate) kw_in: Keyword,
    pub(crate) iterator: Box<Expression>,
    pub(crate) block: BlockExpr,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GroupedExpr {
    pub(crate) inner_expression: Box<Expression>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfExpr {
    pub(crate) kw_if: Keyword,
    pub(crate) condition: Box<GroupedExpr>,
    pub(crate) if_block: Box<BlockExpr>,
    pub(crate) else_if_blocks_opt: Option<Vec<Box<IfExpr>>>,
    pub(crate) trailing_else_block_opt: Option<BlockExpr>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexExpr {
    pub(crate) array: Box<AssigneeExpr>,
    pub(crate) index: Box<ValueExpr>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappingExpr {
    pub(crate) pairs_opt: Option<Vec<MappingPair>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchExpr {
    pub(crate) kw_match: Keyword,
    pub(crate) scrutinee: AssigneeExpr, // expression to be matched against
    pub(crate) match_arms_opt: Option<Vec<MatchArm>>,
    pub(crate) final_arm: Box<MatchArm>, // default case
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodCallExpr {
    pub(crate) receiver: Box<AssigneeExpr>,
    pub(crate) method_name: Identifier,
    pub(crate) args_opt: Option<Vec<Expression>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoneExpr {
    pub(crate) kw_none: Keyword,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathExpr {
    pub(crate) path_root: PathRoot,
    pub(crate) tree_opt: Option<Vec<Identifier>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RangeExpr {
    pub(crate) from_expr_opt: Option<Box<AssigneeExpr>>,
    pub(crate) range_op: RangeOp, // `..` or `..=`
    pub(crate) to_expr_opt: Option<Box<AssigneeExpr>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferenceExpr {
    pub(crate) reference_op: ReferenceOp,
    pub(crate) expression: Box<Expression>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResultExpr {
    pub(crate) kw_ok_or_err: Keyword,
    pub(crate) expression: Box<GroupedExpr>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnExpr {
    pub(crate) kw_return: Keyword,
    pub(crate) expression_opt: Option<Box<Expression>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SomeExpr {
    pub(crate) kw_some: Keyword,
    pub(crate) expression: Box<GroupedExpr>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructExpr {
    pub(crate) struct_path: PathExpr,
    pub(crate) struct_fields_opt: Option<Vec<StructField>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleExpr {
    pub(crate) tuple_elements: TupleElements,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleIndexExpr {
    pub(crate) tuple: Box<AssigneeExpr>,
    pub(crate) index: UInt,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeCastExpr {
    pub(crate) value: Box<ValueExpr>,
    pub(crate) type_cast_op: TypeCastOp, // `as`
    pub(crate) new_type: Box<Type>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnderscoreExpr {
    pub(crate) underscore: Identifier,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    pub(crate) unary_op: UnaryOp,
    pub(crate) value_expr: Box<ValueExpr>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnwrapExpr {
    pub(crate) value_expr: Box<ValueExpr>,
    pub(crate) unwrap_op: UnwrapOp,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileExpr {
    pub(crate) kw_while: Keyword,
    pub(crate) condition: Box<GroupedExpr>,
    pub(crate) block: BlockExpr,
    pub(crate) span: Span,
}
