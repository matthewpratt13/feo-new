use super::{BinaryOp, BlockExpr, Delimiter, Expression, GroupedExpr, Keyword, Separator, Type};

///////////////////////////////////////////////////////////////////////////
/// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Struct representing a single arm in a match statement.
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub case: Expression,
    pub guard_opt: Option<(Keyword, GroupedExpr)>, // `if (..)`
    pub fat_arrow: Separator,
    pub logic: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expression,
    pub semicolon: Separator,
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
pub struct MatchStmt {
    pub kw_match: Keyword,
    pub scrutinee: Expression,
    pub open_brace: Delimiter,
    pub arms_opt: Option<Vec<MatchArm>>,
    pub final_arm: MatchArm, // default case
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub kw_while: Keyword,
    pub condition: GroupedExpr,
    pub block: BlockExpr,
}
