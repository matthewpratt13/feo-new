use core::fmt;

use crate::{
    ast::{
        AliasDecl, ConstantDecl, Delimiter, FunctionItem, Identifier, InnerAttr, Keyword,
        OuterAttr, TraitDef, TraitDefItem, Visibility,
    },
    error::ErrorsEmitted,
    parser::Parser,
    span::Position,
    token::Token,
};

use super::{collection, ParseAssociatedItem, ParseDeclItem, ParseDefItem};

impl ParseDefItem for TraitDef {
    fn parse(
        parser: &mut Parser,
        outer_attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TraitDef, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_trait = if let Some(Token::Trait { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Trait)
        } else {
            parser.log_unexpected_token("`trait`");
            Err(ErrorsEmitted)
        }?;

        let trait_name = match parser.next_token() {
            Some(Token::Identifier { name, .. }) => Ok(Identifier::from(&name)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("identifier");
                Err(ErrorsEmitted)
            }
        }?;

        let open_brace = match parser.current_token() {
            Some(Token::LBrace { .. }) => {
                let position = Position::new(parser.current, &parser.stream.span().input());
                parser.next_token();
                Ok(Delimiter::LBrace { position })
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`{`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let inner_attributes_opt = collection::get_attributes(parser, InnerAttr::inner_attr);

        let trait_items_opt = collection::get_associated_items::<TraitDefItem>(parser)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());

                parser.next_token();

                Ok(TraitDef {
                    outer_attributes_opt,
                    visibility,
                    kw_trait,
                    trait_name,
                    inner_attributes_opt,
                    trait_items_opt,
                    span,
                })
            }
            Some(Token::EOF) | None => {
                parser.log_unmatched_delimiter(&open_brace);
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`}`");
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for TraitDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TraitDef")
            .field("outer_attributes_opt", &self.outer_attributes_opt)
            .field("visibility", &self.visibility)
            .field("trait_name", &self.trait_name)
            .field("inner_attributes_opt", &self.inner_attributes_opt)
            .field("trait_items_opt", &self.trait_items_opt)
            .finish()
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
                Ok(TraitDefItem::ConstantDecl(constant_decl))
            }
            Some(Token::Alias { .. }) => {
                let alias_decl = AliasDecl::parse(parser, attributes_opt, visibility)?;
                Ok(TraitDefItem::AliasDecl(alias_decl))
            }
            Some(Token::Func { .. }) => {
                let function_def = FunctionItem::parse(parser, attributes_opt, visibility)?;
                if function_def.block_opt.is_some() {
                    parser.log_error(crate::error::ParserErrorKind::ExtraTokens {
                        token: parser.current_token().cloned(),
                        msg: "Functions in trait definitions cannot have bodies".to_string(),
                    });
                    Err(ErrorsEmitted)
                } else {
                    Ok(TraitDefItem::FunctionItem(function_def))
                }
            }
            _ => {
                parser.log_unexpected_token("`const`, `alias` or `func`");
                Err(ErrorsEmitted)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_trait_def() -> Result<(), ()> {
        let input = r#"
        pub trait Foo {
            #![interface]
            
            #[storage]
            pub const OWNER: h160 = 0x12345123451234512345;
            pub alias NewType;

            #[modifier]
            func only_owner(&mut self, caller: h160) {}
            func transfer(&mut self, to: h160, amount: u256) -> Error {}
            #[view]
            func sender(&self) -> h160 {}
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
