use super::{AssignmentOp, Expression, Keyword, Pattern, Separator, Type};

///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub kw_let: Keyword,
    pub kw_mut_opt: Option<Keyword>,
    pub assignee: Pattern,
    pub type_ann_opt: Option<(Separator, Type)>, // `: Type`
    pub value_opt: Option<(AssignmentOp, Expression)>, // `= value`
}
