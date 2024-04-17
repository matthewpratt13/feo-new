use super::{BinaryOp, Expression, Keyword, Separator, Type};

///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expression,
    pub semicolon_opt: Option<Separator>,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub kw_let: Keyword,
    pub assignee: Expression,
    pub type_ann_opt: Option<(Separator, Type)>, // `: Type`
    pub value_opt: Option<(BinaryOp, Expression)>, // `= value`
    // pub semicolon: Separator,
}
