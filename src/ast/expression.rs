use super::{BinaryOp, Delimiter, Expression, Identifier, Separator};

pub struct BinaryExpr {
    lhs: Box<Expression>,
    op: BinaryOp,
    rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    open_brace: Delimiter,
    body: Vec<Expression>,
    close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    pub object: Box<Expression>,
    pub dot: Separator,
    pub field: Identifier,
}
