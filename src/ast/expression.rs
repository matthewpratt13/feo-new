use super::{BinaryOp, Delimiter, Expression, Identifier, Keyword, Separator, Type, UIntKind};

#[derive(Debug, Clone)]
pub enum ClosureArgs {
    Some(BinaryOp, Vec<Expression>, BinaryOp),
    None(BinaryOp),
}

#[derive(Debug, Clone)]
pub enum PathPrefixType {
    Package,
    Super,
    SelfKw,
    SelfType,
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
    pub args: ClosureArgs,
    pub arrow: Option<Separator>,
    pub return_type: Option<Type>,
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
    pub index: Box<Expression>,
    pub close_bracket: Delimiter,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    pub receiver: Box<Expression>,
    pub dot: Separator,
    pub callee: Box<Expression>,
    pub open_paren: Delimiter,
    pub args: Option<Vec<Expression>>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct PathExpr {
    pub prefix: PathPrefixType,
    pub dbl_colon: Option<Separator>,
    pub suffixes: Option<Vec<Expression>>,
}

#[derive(Debug, Clone)]
pub struct RangeExpr {
    pub from: Option<Box<Expression>>,
    pub op: Separator,
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
    pub object: Box<Expression>,
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
    pub question_mark: Separator,
}
