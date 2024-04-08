use super::{BinaryOp, Delimiter, Expression, Identifier, Separator};

#[derive(Debug, Clone)]
pub struct BinaryOpExpr {
    pub lhs: Box<Expression>,
    pub op: BinaryOp,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    open_brace: Delimiter,
    body: Vec<Expression>,
    close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expression>,
    pub open_paren: Delimiter,
    pub args: Vec<Expression>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    pub object: Box<Expression>,
    pub dot: Separator,
    pub field: Identifier,
}
