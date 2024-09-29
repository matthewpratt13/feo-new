use super::{
    collection, parse_generic_params, parse_where_clause, ParseAssociatedItem, ParseDeclItem,
    ParseDefItem,
};

use crate::{
    ast::{
        AliasDecl, ConstantDecl, FunctionItem, InnerAttr, Keyword, OuterAttr, TraitDef,
        TraitDefItem, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    parser::Parser,
    token::{Token, TokenType},
};

use core::fmt;

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
            parser.emit_unexpected_token(&TokenType::Trait.to_string());
            Err(ErrorsEmitted)
        }?;

        let trait_name = parser.expect_identifier("trait name")?;

        let generic_params_opt = parse_generic_params(parser)?;

        let where_clause_opt = parse_where_clause(parser)?;

        parser.expect_open_brace()?;

        let inner_attributes_opt = collection::get_attributes(parser, InnerAttr::inner_attr);

        let trait_items_opt = collection::get_associated_items::<TraitDefItem>(parser)?;

        let span = parser.get_braced_item_span(first_token.as_ref())?;

        Ok(TraitDef {
            outer_attributes_opt,
            visibility,
            kw_trait,
            trait_name,
            generic_params_opt,
            where_clause_opt,
            inner_attributes_opt,
            trait_items_opt,
            span,
        })
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
                    parser.emit_error(ParserErrorKind::ExtraTokens {
                        token: parser.current_token().cloned(),
                        msg: "Functions in trait definitions cannot have bodies".to_string(),
                    });
                    Err(ErrorsEmitted)
                } else {
                    Ok(TraitDefItem::FunctionItem(function_def))
                }
            }
            _ => {
                parser.emit_unexpected_token(&format!(
                    "{}, {} or {}",
                    TokenType::Const,
                    TokenType::Alias,
                    TokenType::Func
                ));
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // TODO: add test for trait def with generics and where clauses
    // TODO: e.g., `trait Foo<T: Bar, U> where Self: Baz { func foo(a: T, b: U) -> Self {} … }`
}
