use crate::{
    ast::{
        AliasDecl, ConstantDecl, Delimiter, FunctionItem, Identifier, InnerAttr, Keyword,
        OuterAttr, TraitDef, TraitDefItem, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    parser::Parser,
    token::Token,
};

use super::{collection, ParseAssociatedItem, ParseDeclaration, ParseDefinition};

impl ParseDefinition for TraitDef {
    fn parse(
        parser: &mut Parser,
        outer_attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TraitDef, ErrorsEmitted> {
        let kw_trait = if let Some(Token::Trait { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Trait)
        } else {
            parser.log_unexpected_token("`trait`");
            Err(ErrorsEmitted)
        }?;

        let trait_name = match parser.next_token() {
            Some(Token::Identifier { name, .. }) => Ok(Identifier(name)),
            Some(Token::EOF) | None => {
                parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("identifier");
                Err(ErrorsEmitted)
            }
        }?;

        let open_brace = match parser.next_token() {
            Some(Token::LBrace { .. }) => Ok(Delimiter::LBrace),
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

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(&open_brace);
            Err(ErrorsEmitted)
        }?;

        Ok(TraitDef {
            outer_attributes_opt,
            visibility,
            kw_trait,
            trait_name,
            open_brace,
            inner_attributes_opt,
            trait_items_opt,
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
                        token: parser.current_token(),
                        msg: "functions in trait definitions cannot have bodies".to_string(),
                    });
                    Err(ErrorsEmitted)
                } else {
                    Ok(TraitDefItem::FunctionDef(function_def))
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
