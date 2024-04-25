use super::{AssignmentOp, Expression, Keyword, Pattern, Separator, Type};

///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub kw_let: Keyword,
    pub assignee: Pattern,
    pub type_ann_opt: Option<(Separator, Type)>, // `: Type`
    pub assignment_opt: Option<(AssignmentOp, Expression)>, // `= value`
}
