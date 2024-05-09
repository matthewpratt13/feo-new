mod alias_decl;
mod constant_decl;
mod enum_def;
mod function_item;
mod impl_def;
mod import_decl;
mod module_item;
mod static_item_decl;
mod struct_def;
mod trait_def;

use crate::{
    ast::{
        AliasDecl, ConstantDecl, EnumDef, FunctionItem, ImportDecl, InherentImplDef, Item,
        ModuleItem, OuterAttr, Statement, StaticItemDecl, StructDef, TraitDef, TraitImplDef,
        TupleStructDef, Visibility,
    },
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    token::Token,
};

pub(crate) use super::parse::{ParseAssociatedItem, ParseDeclaration, ParseDefinition};
use super::{collection, ParseStatement, Parser};

impl Item {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<Item, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::Import { .. }) => Ok(Item::ImportDecl(ImportDecl::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            Some(Token::Alias { .. }) => Ok(Item::AliasDecl(AliasDecl::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            Some(Token::Const { .. }) => Ok(Item::ConstantDecl(ConstantDecl::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            Some(Token::Static { .. }) => Ok(Item::StaticItemDecl(StaticItemDecl::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            Some(Token::Module { .. }) => Ok(Item::ModuleItem(Box::new(ModuleItem::parse(
                parser,
                attributes_opt,
                visibility,
            )?))),
            Some(Token::Trait { .. }) => Ok(Item::TraitDef(TraitDef::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            Some(Token::Enum { .. }) => Ok(Item::EnumDef(EnumDef::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            Some(Token::Struct { .. }) => Ok(Item::StructDef(StructDef::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            Some(Token::Impl { .. }) => {
                if let Some(Token::For { .. }) = parser.peek_ahead_by(2) {
                    Ok(Item::TraitImplDef(TraitImplDef::parse(
                        parser,
                        attributes_opt,
                        visibility,
                    )?))
                } else {
                    Ok(Item::InherentImplDef(InherentImplDef::parse(
                        parser,
                        attributes_opt,
                        visibility,
                    )?))
                }
            }
            Some(Token::Func { .. }) => Ok(Item::FunctionItem(FunctionItem::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            _ => {
                parser.log_unexpected_token("item declaration or definition");
                Err(ErrorsEmitted)
            }
        }
    }
}

impl ParseStatement for Item {
    /// Parse the current token and convert it from an `Item` to a `Statement`.
    fn parse_statement(parser: &mut Parser) -> Result<Statement, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `Item::parse_statement()`"),
        );
        parser.log_current_token(true);

        let attributes_opt = collection::get_attributes(parser, OuterAttr::outer_attr);

        let visibility = Visibility::visibility(parser)?;

        let token = parser.current_token();

        match token {
            Some(Token::Import { .. }) => Ok(Statement::Item(Item::ImportDecl(ImportDecl::parse(
                parser,
                attributes_opt,
                visibility,
            )?))),
            Some(Token::Alias { .. }) => Ok(Statement::Item(Item::AliasDecl(AliasDecl::parse(
                parser,
                attributes_opt,
                visibility,
            )?))),
            Some(Token::Const { .. }) => Ok(Statement::Item(Item::ConstantDecl(
                ConstantDecl::parse(parser, attributes_opt, visibility)?,
            ))),
            Some(Token::Static { .. }) => Ok(Statement::Item(Item::StaticItemDecl(
                StaticItemDecl::parse(parser, attributes_opt, visibility)?,
            ))),
            Some(Token::Module { .. }) => Ok(Statement::Item(Item::ModuleItem(Box::new(
                ModuleItem::parse(parser, attributes_opt, visibility)?,
            )))),
            Some(Token::Trait { .. }) => Ok(Statement::Item(Item::TraitDef(TraitDef::parse(
                parser,
                attributes_opt,
                visibility,
            )?))),
            Some(Token::Enum { .. }) => Ok(Statement::Item(Item::EnumDef(EnumDef::parse(
                parser,
                attributes_opt,
                visibility,
            )?))),
            Some(Token::Struct { .. }) => match parser.peek_ahead_by(2) {
                Some(Token::LBrace { .. }) => Ok(Statement::Item(Item::StructDef(
                    StructDef::parse(parser, attributes_opt, visibility)?,
                ))),

                Some(Token::LParen { .. }) => Ok(Statement::Item(Item::TupleStructDef(
                    TupleStructDef::parse(parser, attributes_opt, visibility)?,
                ))),

                _ => {
                    parser.log_unexpected_token("`{` or `(`");
                    Err(ErrorsEmitted)
                }
            },

            Some(Token::Func { .. }) => Ok(Statement::Item(Item::FunctionItem(
                FunctionItem::parse(parser, attributes_opt, visibility)?,
            ))),
            Some(Token::Impl { .. }) => match parser.peek_ahead_by(2) {
                Some(Token::For { .. }) => Ok(Statement::Item(Item::TraitImplDef(
                    TraitImplDef::parse(parser, attributes_opt, visibility)?,
                ))),
                Some(Token::LBrace { .. }) => Ok(Statement::Item(Item::InherentImplDef(
                    InherentImplDef::parse(parser, attributes_opt, visibility)?,
                ))),
                _ => {
                    parser.log_unexpected_token("`for` or `{`");
                    Err(ErrorsEmitted)
                }
            },
            _ => {
                parser.log_unexpected_token("item declaration or definition item");
                Err(ErrorsEmitted)
            }
        }
    }
}
