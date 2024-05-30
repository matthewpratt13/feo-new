use crate::span::Span;

use super::{Expression, IdentifierPatt, Keyword, Type};

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub(crate) kw_let: Keyword,
    pub(crate) assignee: IdentifierPatt,
    pub(crate) type_ann_opt: Option<Type>,
    pub(crate) value_opt: Option<Expression>,
    pub(crate) span: Span,
}
