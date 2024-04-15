use super::{BinaryOp, BlockExpr, Expression, GroupedExpr, Keyword, Separator, Type};

///////////////////////////////////////////////////////////////////////////
/// HELPER TYPES
///////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////
/// NODES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expression,
    pub semicolon_opt: Option<Separator>,
}

#[derive(Debug, Clone)]
pub struct ForInStmt {
    pub kw_for: Keyword,
    pub assignee: Expression,
    pub kw_in: Keyword,
    pub iterable: Expression,
    pub block: BlockExpr,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub kw_if: Keyword,
    pub condition: GroupedExpr,
    pub if_block: BlockExpr,
    pub else_if_blocks_opt: Option<Vec<(Keyword, Box<IfStmt>)>>, // `else`, `if { .. }`
    pub trailing_else_block_opt: Option<(Keyword, BlockExpr)>,   // `else { .. }`
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub kw_let: Keyword,
    pub assignee: Expression,
    pub type_ann_opt: Option<(Separator, Type)>, // `: Type`
    pub value_opt: Option<(BinaryOp, Expression)>, // `= value`
    pub semicolon: Separator,
}


#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub kw_while: Keyword,
    pub condition: GroupedExpr,
    pub block: BlockExpr,
}
