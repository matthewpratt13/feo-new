use super::{Expression, Keyword, Pattern, Type};

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub kw_let: Keyword,
    pub assignee: Pattern,
    pub type_ann_opt: Option<Type>,    
    pub value_opt: Option<Expression>, 
}
