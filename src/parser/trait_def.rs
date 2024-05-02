use crate::{
    ast::{
        AliasDecl, ConstantDecl, Delimiter, FunctionItem, Identifier, InnerAttr, OuterAttr,
        TraitDef, TraitDefItem, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{
    collection,
    item::{ParseAssociatedItem, ParseDeclaration, ParseDefinition},
    Parser,
};

impl ParseDefinition for TraitDef {
    fn parse(
        parser: &mut Parser,
        outer_attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TraitDef, ErrorsEmitted> {
        let kw_trait = parser.expect_keyword(TokenType::Trait)?;

        let trait_name = if let Some(Token::Identifier { name, .. }) = parser.current_token() {
            parser.next_token();
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let inner_attributes_opt = collection::get_attributes(parser, InnerAttr::inner_attr);

        let trait_items = collection::get_associated_items::<TraitDefItem>(parser)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

        Ok(TraitDef {
            outer_attributes_opt,
            visibility,
            kw_trait,
            trait_name,
            open_brace,
            inner_attributes_opt,
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

impl ParseAssociatedItem for TraitDefItem {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TraitDefItem, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::Const { .. }) => {
                let constant_decl = ConstantDecl::parse(parser, attributes_opt, visibility)?;
                parser.next_token();
                Ok(TraitDefItem::ConstantDecl(constant_decl))
            }
            Some(Token::Alias { .. }) => {
                let alias_decl = AliasDecl::parse(parser, attributes_opt, visibility)?;
                parser.next_token();
                Ok(TraitDefItem::AliasDecl(alias_decl))
            }
            Some(Token::Func { .. }) => {
                let function_def = FunctionItem::parse(parser, attributes_opt, visibility)?;
                parser.next_token();
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
