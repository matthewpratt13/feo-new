mod alias_decl;
mod constant_decl;
mod enum_def;
mod function_item;
mod impl_def;
mod import_decl;
mod module_item;
mod static_var_decl;
mod struct_def;
mod trait_def;

pub(crate) use super::parse::{ParseAssociatedItem, ParseDeclItem, ParseDefItem};

use super::{collection, ParseStatement, Parser};

use crate::{
    ast::{
        AliasDecl, ConstantDecl, Delimiter, EnumDef, FunctionItem, GenericParam, GenericParams,
        Identifier, ImportDecl, InherentImplDef, Item, Keyword, ModuleItem, OuterAttr, Statement,
        StaticVarDecl, StructDef, TraitDef, TraitImplDef, TupleStructDef, Visibility,
    },
    error::ErrorsEmitted,
    span::Span,
    token::Token,
};

impl Item {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<Item, ErrorsEmitted> {
        parser.logger.debug("entering `Item::parse()`");
        parser.log_current_token(false);

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
            Some(Token::Static { .. }) => Ok(Item::StaticVarDecl(StaticVarDecl::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
            Some(Token::Module { .. }) => Ok(Item::ModuleItem(ModuleItem::parse(
                parser,
                attributes_opt,
                visibility,
            )?)),
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
            Some(Token::EOF) => Ok(Item::EnumDef(EnumDef {
                attributes_opt,
                visibility,
                kw_enum: Keyword::Anonymous,
                enum_name: Identifier::from(""),
                generic_params_opt: None,
                variants: Vec::new(),
                span: Span::default(),
            })),

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
        parser.logger.debug("entering `Item::parse_statement()`");
        parser.log_current_token(false);

        let attributes_opt = collection::get_attributes(parser, OuterAttr::outer_attr);

        let visibility = Visibility::visibility(parser)?;

        match parser.current_token() {
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
            Some(Token::Static { .. }) => Ok(Statement::Item(Item::StaticVarDecl(
                StaticVarDecl::parse(parser, attributes_opt, visibility)?,
            ))),
            Some(Token::Module { .. }) => Ok(Statement::Item(Item::ModuleItem(ModuleItem::parse(
                parser,
                attributes_opt,
                visibility,
            )?))),
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
            None | Some(Token::EOF { .. }) => {
                Err(parser.log_missing_token("item definition keyword"))
            }
            Some(_) => {
                if attributes_opt.is_some() {
                    return Err(parser.log_unexpected_token("item to match attributes"));
                }

                Err(parser
                    .log_unexpected_token("module definition, or implementation or trait item"))
            }
        }
    }
}

pub(crate) fn parse_generic_params(
    parser: &mut Parser,
) -> Result<Option<GenericParams>, ErrorsEmitted> {
    if let Some(Token::LessThan { .. }) = parser.current_token() {
        let position = parser.current_position();
        let left_angle_bracket = Delimiter::LAngleBracket { position };

        parser.next_token();

        let generics =
            collection::get_collection(parser, parse_generic_param, &left_angle_bracket)?
                .ok_or_else(|| {
                    parser.log_missing("ty", "generic params");
                    ErrorsEmitted
                })?;

        match parser.current_token() {
            Some(Token::GreaterThan { .. }) => {
                parser.next_token();
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                return Err(ErrorsEmitted);
            }
            _ => {
                parser.log_unexpected_token("`>`");
                return Err(ErrorsEmitted);
            }
        }

        Ok(Some(GenericParams { params: generics }))
    } else {
        Ok(None)
    }
}

pub(crate) fn parse_generic_param(parser: &mut Parser) -> Result<GenericParam, ErrorsEmitted> {
    let name = match parser.current_token() {
        Some(Token::Identifier { name, .. }) => {
            if name.len() == 1
                && name
                    .as_str()
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_uppercase() || c == '_')
            {
                Identifier::from(name)
            } else {
                parser.log_unexpected_token("single uppercase alphabetic character or `_`");
                return Err(ErrorsEmitted);
            }
        }

        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            return Err(ErrorsEmitted);
        }

        _ => {
            parser.log_unexpected_token("identifier");
            return Err(ErrorsEmitted);
        }
    };

    let param_bound_opt = if let Some(Token::Colon { .. }) = parser.peek_ahead_by(1) {
        parser.next_token();
        parser.next_token();

        match parser.current_token() {
            Some(Token::Identifier { name, .. }) => Some(Identifier::from(name)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                return Err(ErrorsEmitted);
            }
            _ => {
                parser.log_unexpected_token("identifier");
                return Err(ErrorsEmitted);
            }
        }
    } else {
        None
    };

    Ok(GenericParam {
        name,
        param_bound_opt,
    })
}
