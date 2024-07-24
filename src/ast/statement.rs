use core::fmt;

use crate::span::Span;

use super::{Expression, IdentifierPatt, Keyword, Type};

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq, Eq)]
pub struct LetStmt {
    pub(crate) kw_let: Keyword,
    pub(crate) assignee: IdentifierPatt,
    pub(crate) type_ann_opt: Option<Type>,
    pub(crate) value_opt: Option<Expression>,
    pub(crate) span: Span,
}

impl fmt::Debug for LetStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LetStmt")
            .field("assignee", &self.assignee)
            .field("type_ann_opt", &self.type_ann_opt)
            .field("value_opt", &self.value_opt)
            .finish()
    }
}
