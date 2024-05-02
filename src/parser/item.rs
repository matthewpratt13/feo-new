use crate::{
    ast::{
        AliasDecl, ConstantDecl, EnumDef, FunctionItem, ImportDecl, InherentImplDef, Item,
        ModuleItem, OuterAttr, StaticItemDecl, StructDef, TraitDef, TraitImplDef, Visibility,
    },
    error::ErrorsEmitted,
    token::Token,
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

pub(crate) trait ParseAssociatedItem
where
    Self: Sized,
{
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<Self, ErrorsEmitted>;
}

impl ParseAssociatedItem for Item {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<Item, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::Import { .. }) => Ok(Item::ImportDecl(ImportDecl::parse(
                parser, attributes, visibility,
            )?)),
            Some(Token::Alias { .. }) => Ok(Item::AliasDecl(AliasDecl::parse(
                parser, attributes, visibility,
            )?)),
            Some(Token::Const { .. }) => Ok(Item::ConstantDecl(ConstantDecl::parse(
                parser, attributes, visibility,
            )?)),
            Some(Token::Static { .. }) => Ok(Item::StaticItemDecl(StaticItemDecl::parse(
                parser, attributes, visibility,
            )?)),
            Some(Token::Module { .. }) => Ok(Item::ModuleItem(Box::new(ModuleItem::parse(
                parser, attributes, visibility,
            )?))),
            Some(Token::Trait { .. }) => Ok(Item::TraitDef(TraitDef::parse(
                parser, attributes, visibility,
            )?)),
            Some(Token::Enum { .. }) => Ok(Item::EnumDef(EnumDef::parse(
                parser, attributes, visibility,
            )?)),
            Some(Token::Struct { .. }) => Ok(Item::StructDef(StructDef::parse(
                parser, attributes, visibility,
            )?)),
            Some(Token::Impl { .. }) => {
                if let Some(Token::For { .. }) = parser.peek_ahead_by(2) {
                    Ok(Item::TraitImplDef(TraitImplDef::parse(
                        parser, attributes, visibility,
                    )?))
                } else {
                    Ok(Item::InherentImplDef(InherentImplDef::parse(
                        parser, attributes, visibility,
                    )?))
                }
            }
            Some(Token::Func { .. }) => Ok(Item::FunctionItem(FunctionItem::parse(
                parser, attributes, visibility,
            )?)),
            _ => {
                parser.log_unexpected_str("declaration or definition");
                Err(ErrorsEmitted)
            }
        }
    }
}
