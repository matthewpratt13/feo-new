use crate::{
    ast::{Expression, OuterAttr, Statement, Visibility},
    error::ErrorsEmitted,
};

use super::Parser;

pub(crate) trait ParseOperation {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted>;
}

pub(crate) trait ParseConstruct {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted>;
}

pub(crate) trait ParseControl {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted>;
}

/// Trait that defines a shared interface for declaration type `Item`.
/// E.g., `ConstantDecl` and `ImportDecl`.
pub(crate) trait ParseDeclaration
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
pub(crate) trait ParseDefinition
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
pub(crate) trait ParseStatement {
    fn parse_statement(parser: &mut Parser) -> Result<Statement, ErrorsEmitted>;
}
