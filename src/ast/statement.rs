use super::{AssigneeExpr, AssignmentOp, Expression, Keyword, Separator, Type};

///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub kw_let: Keyword,
    pub kw_mut_opt: Option<Keyword>,
    pub assignee: AssigneeExpr,
    pub type_ann_opt: Option<(Separator, Type)>, // `: Type`
    pub value_opt: Option<(AssignmentOp, Expression)>, // `= value`
}
