use super::{Expression, Keyword, Pattern, Type};

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub(crate) kw_let: Keyword,
    pub(crate) assignee: Pattern,
    pub(crate) type_ann_opt: Option<Type>,
    pub(crate) value_opt: Option<Expression>,
}
