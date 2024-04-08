use super::{BinaryOp, Delimiter, Expression};

pub struct BinaryExpr {
    lhs: Box<Expression>,
    op: BinaryOp,
    rhs: Box<Expression>,
}

pub struct BlockExpr {
    open_brace: Delimiter,
    body: Vec<Expression>,
    close_brace: Delimiter,
}
