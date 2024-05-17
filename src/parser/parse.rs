use crate::{
    ast::{Expression, OuterAttr, Statement, Visibility},
    error::ErrorsEmitted,
};

use super::Parser;

pub(crate) trait ParseOperatorExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted>;
}

pub(crate) trait ParseConstructExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

pub(crate) trait ParseControlExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

pub(crate) trait ParseSimpleExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

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
pub trait ParseAssociatedItem
where
    Self: Sized,
{
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<Self, ErrorsEmitted>;
}

pub trait ParseStatement {
    fn parse_statement(parser: &mut Parser) -> Result<Statement, ErrorsEmitted>;
}
