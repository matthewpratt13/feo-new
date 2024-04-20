use crate::{
    ast::{
        ConstantDecl, FunctionDef, InherentImplDef, InherentImplItem, OuterAttr, TraitImplDef,
        Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{item::ParseDeclaration, ParseDefinition, Parser};

impl ParseDefinition for InherentImplDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<InherentImplDef, ErrorsEmitted> {
        let kw_impl = parser.expect_keyword(Token::Impl {
            name: "impl".to_string(),
            span: parser.stream.span(),
        })?;

        let mut associated_items: Vec<InherentImplItem> = Vec::new();

        let nominal_type = parser.get_type()?;

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let mut item_attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                item_attributes.push(oa);
                parser.consume_token();
            }

            let item_visibility = parser.get_visibility()?;

            let token = parser.peek_current();

            let associated_item = if let Some(Token::Const { .. }) = token {
                Ok(InherentImplItem::ConstantDecl(ConstantDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else if let Some(Token::Func { .. }) = token {
                Ok(InherentImplItem::FunctionDef(FunctionDef::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`const`,`func`".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }?;

            associated_items.push(associated_item);
        }

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(InherentImplDef {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            kw_impl,
            nominal_type,
            open_brace,
            associated_items_opt: {
                if associated_items.is_empty() {
                    None
                } else {
                    Some(associated_items)
                }
            },
            close_brace,
        })
    }
}

impl ParseDefinition for TraitImplDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TraitImplDef, ErrorsEmitted> {
        todo!()
    }
}
