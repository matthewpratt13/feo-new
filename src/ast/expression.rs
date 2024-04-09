use super::{BinaryOp, Delimiter, Expression, Identifier, Keyword, Separator, Type, UIntKind};

#[derive(Debug, Clone)]
pub enum ClosureParams {
    Some(BinaryOp, Vec<Param>, BinaryOp),
    None(BinaryOp),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub id: Identifier,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum PathPrefix {
    Package,
    Super,
    SelfKw,
    SelfType,
    Identifier(Identifier),
}

#[derive(Debug, Clone)]
pub struct BinaryOpExpr {
    pub lhs: Box<Expression>,
    pub op: BinaryOp,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub open_brace: Delimiter,
    pub expressions: Vec<Expression>,
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
    pub args: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct ClosureExpr {
    pub params: ClosureParams,
    pub return_type: Option<(Separator, Type)>, // `-> Type`
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
pub struct GroupedExpr {
    pub open_paren: Delimiter,
    pub expression: Box<Expression>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub array: Box<Expression>,
    pub open_bracket: Delimiter,
    pub index: UIntKind,
    pub close_bracket: Delimiter,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub receiver: Box<Expression>,
    pub dot: Separator,
    pub method_name: Identifier,
    pub open_paren: Delimiter,
    pub args: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct PathExpr {
    pub root: PathPrefix,
    pub tree: Option<Vec<Identifier>>,
    pub suffix: Option<Separator> // `::*`
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub from: Option<Box<Expression>>,
    pub op: Separator, // `..` or `..=`
    pub to: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ReturnExpr {
    pub kw_return: Keyword,
    pub expression: Box<Expression>,
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
pub struct UnwrapExpr {
    pub expression: Box<Expression>,
    pub op: Separator, // `?`
}
