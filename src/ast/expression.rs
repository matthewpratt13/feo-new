use super::{
    BinaryOp, Delimiter, Expression, Identifier, Keyword, RangeOp, Separator, Statement, Type,
    UIntKind, UnwrapOp,
};

///////////////////////////////////////////////////////////////////////////
/// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Enum representing whether or not a closure has parameters in its definition.
#[derive(Debug, Clone)]
pub enum ClosureParams {
    Some(BinaryOp, Vec<Param>, BinaryOp),
    None(BinaryOp),
}

/// Enum representing the different path root options.
#[derive(Debug, Clone)]
pub enum PathPrefix {
    Package,
    Super,
    SelfKw,
    SelfType,
    Identifier(Identifier),
}

/// Struct representing a function or closure parameter.
#[derive(Debug, Clone)]
pub struct Param {
    pub id: Identifier,
    pub ty: Option<Type>,
}

/// Struct representing a single field in a struct expression, with a name and value.
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Identifier,
    pub value: Expression,
}

///////////////////////////////////////////////////////////////////////////
/// AST NODES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct ArrayExpr {
    pub open_bracket: Delimiter,
    pub elements: Vec<Expression>,
    pub close_bracket: Delimiter,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub lhs: Box<Expression>,
    pub op: BinaryOp,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub open_brace: Delimiter,
    pub statements: Vec<Statement>,
    pub terminal_expression_opt: Option<Box<Expression>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct BreakExpr {
    pub kw_break: Keyword,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expression>,
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
pub struct ContinueExpr {
    pub kw_continue: Keyword,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    pub object: Box<Expression>,
    pub dot: Separator,
    pub field: Identifier,
}

#[derive(Debug, Clone)]
pub struct ForInStmt {
    pub kw_for: Keyword,
    pub assignee: Expression,
    pub kw_in: Keyword,
    pub iterable: Expression,
    pub block: Expression,
}

#[derive(Debug, Clone)]
pub struct GroupedExpr {
    pub open_paren: Delimiter,
    pub expression: Box<Expression>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub kw_if: Keyword,
    pub condition: Expression,
    pub if_block: BlockExpr,
    pub else_if_blocks_opt: Option<Vec<(Keyword, Box<IfStmt>)>>, // `else`, `if { .. }`
    pub trailing_else_block_opt: Option<(Keyword, BlockExpr)>,   // `else { .. }`
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub array: Box<Expression>,
    pub open_bracket: Delimiter,
    pub index: UIntKind,
    pub close_bracket: Delimiter,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub kw_let: Keyword,
    pub assignee: Expression,
    pub type_ann_opt: Option<(Separator, Type)>, // `: Type`
    pub value_opt: Option<(BinaryOp, Expression)>, // `= value`
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub receiver: Box<Expression>,
    pub dot: Separator,
    pub method_name: Identifier,
    pub open_paren: Delimiter,
    pub args_opt: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct PathExpr {
    pub root: PathPrefix,
    pub tree_opt: Option<Vec<Identifier>>,
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub from_opt: Option<Box<Expression>>,
    pub op: RangeOp, // `..` or `..=`
    pub to: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ReturnExpr {
    pub kw_return: Keyword,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct StructExpr {
    pub path: Box<PathExpr>,
    pub open_brace: Delimiter,
    pub fields: Vec<StructField>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct TernaryStmt {
    pub kw_if: Keyword,
    pub condition: Expression,
    pub question_mark: UnwrapOp,
    pub true_case: Expression,
    pub colon: Separator,
    pub false_case: Expression,
    pub semicolon: Separator,
}

#[derive(Debug, Clone)]
pub struct TupleExpr {
    pub open_paren: Delimiter,
    pub elements: Vec<Expression>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct TupleIndexExpr {
    pub operand: Box<Expression>,
    pub dot: Separator,
    pub index: UIntKind,
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
    pub expression: Box<Expression>,
    pub op: UnwrapOp,
}
