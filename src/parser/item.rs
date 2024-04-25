use crate::{
    ast::{OuterAttr, Visibility},
    error::ErrorsEmitted,
};

use super::Parser;

/// Trait that defines a shared interface for declaration type `Item`.
/// E.g., `ConstantDecl` and `ImportDecl`.
pub(crate) trait ParseDeclaration
where
    Self: Sized,
{
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
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
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<Self, ErrorsEmitted>;
}
