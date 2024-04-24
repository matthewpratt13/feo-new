use crate::{
    ast::{
        AliasDecl, ConstantDecl, Delimiter, FunctionItem, Identifier, InnerAttr, OuterAttr,
        TraitDef, TraitDefItem, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{
    item::{ParseDeclaration, ParseDefinition},
    Parser,
};

impl ParseDefinition for TraitDef {
    fn parse(
        parser: &mut Parser,
        outer_attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TraitDef, ErrorsEmitted> {
        let kw_trait = parser.expect_keyword(Token::Trait {
            name: "trait".to_string(),
            span: parser.stream.span(),
        })?;

        let mut associated_items: Vec<TraitDefItem> = Vec::new();
        let mut inner_attributes: Vec<InnerAttr> = Vec::new();

        let token = parser.consume_token();

        let trait_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("identifier".to_string());
            Err(ErrorsEmitted(()))
        }?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        while let Some(ia) = parser.get_inner_attr() {
            inner_attributes.push(ia);
            parser.consume_token();
        }

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
                Ok(TraitDefItem::ConstantDecl(ConstantDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else if let Some(Token::Alias { .. }) = token {
                Ok(TraitDefItem::AliasDecl(AliasDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else if let Some(Token::Func { .. }) = token {
                Ok(TraitDefItem::FunctionDef(FunctionItem::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`const`, `alias` or `func`".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }?;

            associated_items.push(associated_item);
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

        Ok(TraitDef {
            outer_attributes_opt: {
                if outer_attributes.is_empty() {
                    None
                } else {
                    Some(outer_attributes)
                }
            },
            visibility,
            kw_trait,
            trait_name,
            open_brace,
            inner_attributes_opt: {
                if inner_attributes.is_empty() {
                    None
                } else {
                    Some(inner_attributes)
                }
            },
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

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_trait_def() -> Result<(), ()> {
        let input = r#"
        pub trait Foo {
            #![interface]
            
            #[storage]
            pub const OWNER: h160 = 0x12345123451234512345;
            pub alias NewType;

            #[modifier]
            func only_owner(&mut self, caller: h160);
            func transfer(&mut self, to: h160, amount: u256) -> Error;
            #[view]
            func sender(&self) -> h160;
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
