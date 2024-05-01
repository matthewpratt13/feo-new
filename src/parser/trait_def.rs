use crate::{
    ast::{
        AliasDecl, ConstantDecl, Delimiter, FunctionItem, Identifier, InnerAttr, OuterAttr, TraitDef, TraitDefItem, Visibility
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
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
        let kw_trait = parser.expect_keyword(TokenType::Trait)?;

        let trait_name = if let Some(Token::Identifier { name, .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let mut trait_items = Vec::new();
        let mut inner_attributes: Vec<InnerAttr> = Vec::new();

        while let Some(ia) = parser.get_inner_attr() {
            inner_attributes.push(ia);
            parser.consume_token();
        }

        while !matches!(
            parser.peek_current(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            let mut item_attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                item_attributes.push(oa);
                parser.consume_token();
            }

            let item_visibility = parser.get_visibility()?;

            let trait_item = TraitDefItem::parse(parser, item_attributes, item_visibility)?;
            trait_items.push(trait_item);
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
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
            trait_items_opt: {
                if trait_items.is_empty() {
                    None
                } else {
                    Some(trait_items)
                }
            },
            close_brace,
        })
    }
}

impl TraitDefItem {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TraitDefItem, ErrorsEmitted> {
        match parser.peek_current() {
            Some(Token::Const { .. }) => {
                let constant_decl = ConstantDecl::parse(parser, attributes, visibility)?;
                parser.consume_token();
                Ok(TraitDefItem::ConstantDecl(constant_decl))
            }
            Some(Token::Alias { .. }) => {
                let alias_decl = AliasDecl::parse(parser, attributes, visibility)?;
                parser.consume_token();
                Ok(TraitDefItem::AliasDecl(alias_decl))
            }
            Some(Token::Func { .. }) => {
                let function_def = FunctionItem::parse(parser, attributes, visibility)?;
                parser.consume_token();
                Ok(TraitDefItem::FunctionDef(function_def))
            }
            _ => {
                parser.log_unexpected_str("`const`, `alias` or `func`");
                Err(ErrorsEmitted)
            }
        }
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
