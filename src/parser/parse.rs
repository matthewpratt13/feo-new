use super::Parser;

use crate::{
    ast::{Expression, OuterAttr, Statement, Visibility},
    error::ErrorsEmitted,
};

///////////////////////////////////////////////////////////////////////////
// EXPRESSION INTERFACES
///////////////////////////////////////////////////////////////////////////

/// Trait that defines a shared interface for construct expressions.
/// E.g., `GroupedExpr`, `ArrayExpr`, `StructExpr`, `TupleExpr`, etc.
pub(crate) trait ParseConstructExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

/// Trait that defines a shared interface for control expressions.
/// I.e., `IfExpr`, `MatchExpr`, `WhileExpr`, and `ForInExpr`.
pub(crate) trait ParseControlExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

/// Trait that defines a shared interface for operator expressions.
/// E.g., `BinaryExpr`, `AssignmentExpr`, `FieldAccessExpr`, `CallExpr`, etc.
pub(crate) trait ParseOperatorExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted>;
}

/// Trait that defines a shared interface for other basic expressions.
/// I.e., `PathExpr`, `UnaryExpr`, `ReferenceExpr`, `DereferenceExpr`
pub(crate) trait ParseSimpleExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

///////////////////////////////////////////////////////////////////////////
// ITEM INTERFACES
///////////////////////////////////////////////////////////////////////////

/// Trait that defines a shared interface for declaration type `Item`.
/// E.g., `ConstantDecl` and `ImportDecl`.
pub(crate) trait ParseDeclItem
where
    Self: Sized,
{
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<Self, ErrorsEmitted>;
}

/// Trait that defines a shared interface for definition type `Item`.
/// E.g., `StructDef` and `TraitDef`.
pub(crate) trait ParseDefItem
where
    Self: Sized,
{
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<Self, ErrorsEmitted>;
}

/// Trait that defines a shared interface for associated items – i.e., an `Item` that is associated
/// with traits and implementations. E.g., `TraitDefItem` and `InherentImplItem`.
pub(crate) trait ParseAssociatedItem
where
    Self: Sized,
{
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<Self, ErrorsEmitted>;
}

///////////////////////////////////////////////////////////////////////////
// STATEMENT INTERFACES
///////////////////////////////////////////////////////////////////////////

/// Trait that defines a shared interface for statements.
/// I.e., `LetStmt` and `Item`.
pub(crate) trait ParseStatement {
    fn parse_statement(parser: &mut Parser) -> Result<Statement, ErrorsEmitted>;
}

///////////////////////////////////////////////////////////////////////////
// PATTERN INTERFACES
///////////////////////////////////////////////////////////////////////////

/// Trait that defines a shared interface for patterns.
/// E.g., `PathPatt`, `GroupedPatt`, `RangePatt` and `ReferencePatt`.
pub(crate) trait ParsePattern
where
    Self: Sized,
{
    fn parse_patt(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}
