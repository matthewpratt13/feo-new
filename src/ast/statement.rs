use super::{AssignmentOp, Expression, Keyword, PlaceExpr, Separator, Type};

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
    pub kw_mut_opt: Option<Keyword>,
    pub assignee: PlaceExpr,
    pub type_ann_opt: Option<(Separator, Type)>, // `: Type`
    pub value_opt: Option<(AssignmentOp, Expression)>, // `= value`
}
