use super::{
    collection, parse_generic_params, parse_where_clause, ParseAssociatedItem, ParseDeclItem,
    ParseDefItem, Parser,
};

use crate::{
    ast::{
        AliasDecl, ConstantDecl, FunctionItem, InherentImplDef, InherentImplItem, Keyword,
        OuterAttr, TraitImplDef, TraitImplItem, Type, TypePath, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use core::fmt;

impl ParseDefItem for InherentImplDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        _visibility: Visibility,
    ) -> Result<InherentImplDef, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_impl = if let Some(Token::Impl { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Impl)
        } else {
            parser.log_unexpected_token("`impl`");
            Err(ErrorsEmitted)
        }?;

        let generic_declaration_opt = parse_generic_params(parser)?;

        let nominal_type = TypePath::parse(parser, parser.current_token().cloned())?;

        let generic_params_opt = match (generic_declaration_opt, parse_generic_params(parser)?) {
            (None, None) => None,
            (None, Some(ga)) => {
                parser.log_error(ParserErrorKind::UndeclaredGenerics {
                    found: format!("{:?}", ga.params),
                });

                return Err(ErrorsEmitted);
            }
            (Some(ga), None) => {
                parser
                    .logger
                    .warn(&format!("unused generics declared: {:?}", ga.params));
                None
            }
            (Some(_), Some(ga)) => Some(ga),
        };

        let open_brace = parser.expect_delimiter(TokenType::LBrace).and_then(|d| {
            d.ok_or_else(|| {
                parser.logger.warn(&format!(
                    "bad input to `Parser::expect_delimiter()` function. Expected delimiter token, found {:?}",
                    parser.current_token()
                ));
                ErrorsEmitted
            })
        })?;

        let associated_items_opt = collection::get_associated_items::<InherentImplItem>(parser)?;

        let span = parser.get_braced_item_span(first_token.as_ref(), &open_brace)?;

        Ok(InherentImplDef {
            attributes_opt,
            kw_impl,
            nominal_type,
            generic_params_opt,
            associated_items_opt,
            span,
        })
    }
}

impl fmt::Debug for InherentImplDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InherentImplDef")
            .field("attributes_opt", &self.attributes_opt)
            .field("nominal_type", &self.nominal_type)
            .field("associated_items_opt", &self.associated_items_opt)
            .finish()
    }
}

impl ParseDefItem for TraitImplDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        _visibility: Visibility,
    ) -> Result<TraitImplDef, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_impl = if let Some(Token::Impl { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Impl)
        } else {
            parser.log_unexpected_token("`impl`");
            Err(ErrorsEmitted)
        }?;

        let generic_declaration_opt = parse_generic_params(parser)?;

        let token = parser.current_token().cloned();

        let implemented_trait_path = match &token {
            Some(Token::Identifier { .. }) => {
                let path = TypePath::parse(parser, token);
                path
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => {
                parser.log_unexpected_token("implemented trait path");
                parser.next_token();

                Err(ErrorsEmitted)
            }
        }?;

        let implemented_trait_generic_params_opt =
            match (generic_declaration_opt, parse_generic_params(parser)?) {
                (None, None) => None,
                (None, Some(ga)) => {
                    parser.log_error(ParserErrorKind::UndeclaredGenerics {
                        found: format!("{:?}", ga.params),
                    });

                    return Err(ErrorsEmitted);
                }
                (Some(ga), None) => {
                    parser
                        .logger
                        .warn(&format!("unused generics declared: {:?}", ga.params));
                    None
                }
                (Some(_), Some(ga)) => Some(ga),
            };

        let kw_for = parser
            .expect_token(TokenType::For)
            .and_then(|_| Ok(Keyword::For))?;

        let implementing_type = Type::parse(parser)?;

        let implementing_type_generic_params_opt = parse_generic_params(parser)?;

        let where_clause_opt = parse_where_clause(parser)?;

        let open_brace = parser.expect_delimiter(TokenType::LBrace).and_then(|d| {
            d.ok_or_else(|| {
                parser.logger.warn(&format!(
                    "bad input to `Parser::expect_delimiter()` function. Expected delimiter token, found {:?}",
                    parser.current_token()
                ));
                ErrorsEmitted
            })
        })?;

        let associated_items_opt = collection::get_associated_items::<TraitImplItem>(parser)?;

        let span = parser.get_braced_item_span(first_token.as_ref(), &open_brace)?;

        Ok(TraitImplDef {
            attributes_opt,
            kw_impl,
            implemented_trait_path,
            implemented_trait_generic_params_opt,
            kw_for,
            implementing_type,
            implementing_type_generic_params_opt,
            where_clause_opt,
            associated_items_opt,
            span,
        })
    }
}

impl fmt::Debug for TraitImplDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TraitImplDef")
            .field("attributes_opt", &self.attributes_opt)
            .field("implemented_trait_path", &self.implemented_trait_path)
            .field("implementing_type", &self.implementing_type)
            .field("associated_items_opt", &self.associated_items_opt)
            .finish()
    }
}

impl ParseAssociatedItem for InherentImplItem {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<InherentImplItem, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::Const { .. }) => {
                let constant_decl = ConstantDecl::parse(parser, attributes_opt, visibility)?;
                if constant_decl.value_opt.is_none() {
                    parser.logger.warn("assigned value cannot be `None`");
                    Err(ErrorsEmitted)
                } else {
                    Ok(InherentImplItem::ConstantDecl(constant_decl))
                }
            }

            Some(Token::Func { .. }) => {
                let function_def = FunctionItem::parse(parser, attributes_opt, visibility)?;
                if function_def.block_opt.is_none() {
                    parser.log_missing("item", "function implementation");
                    Err(ErrorsEmitted)
                } else {
                    Ok(InherentImplItem::FunctionItem(function_def))
                }
            }
            _ => {
                parser.log_unexpected_token("`const` or `func`");
                Err(ErrorsEmitted)
            }
        }
    }
}

impl ParseAssociatedItem for TraitImplItem {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TraitImplItem, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::Const { .. }) => {
                let constant_decl = ConstantDecl::parse(parser, attributes_opt, visibility)?;
                if constant_decl.value_opt.is_none() {
                    parser.logger.warn("assigned value cannot be `None`");
                    Err(ErrorsEmitted)
                } else {
                    Ok(TraitImplItem::ConstantDecl(constant_decl))
                }
            }
            Some(Token::Alias { .. }) => {
                let alias_decl = AliasDecl::parse(parser, attributes_opt, visibility)?;
                Ok(TraitImplItem::AliasDecl(alias_decl))
            }
            Some(Token::Func { .. }) => {
                let function_def = FunctionItem::parse(parser, attributes_opt, visibility)?;
                if function_def.block_opt.is_none() {
                    parser.log_missing("item", "trait implementation associated item");
                    Err(ErrorsEmitted)
                } else {
                    Ok(TraitImplItem::FunctionItem(function_def))
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
    fn parse_impl_def_inherent() -> Result<(), ()> {
        let input = r#"
        impl Foo {
            #[storage]
            const BAR: str = "bar";

            #[constructor]
            pub func new(param1: char, param2: bool) -> Foo {
                Foo {
                    param1: param1,
                    param2: param2
                }
            }

            #[view]
            pub func get_param1(&self) -> char {
                self.param1
            }

            func set_param2(&mut self) {
                self.param2 = false
            }
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_impl_def_trait() -> Result<(), ()> {
        let input = r#"
        impl Foo for Bar {
            #[storage]
            const BAZ: str = "baz";
            alias FooBar = u64;

            pub func sender(&self) -> h160 {
                self.address
            }

            pub func transfer(&mut self, to: h160, amount: u256) -> Event {
                self.balance -= amount;
                to.balance += amount;

                Event { msg: "transfer", to: to, amount: amount }
            }
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    // TODO: add test for implementation def with generics and where clauses
    // TODO: e.g., `impl<T: Bar, U, V: Baz> Foo<T, U, V> { func foo(a: T, B: U) -> V }`
    // TODO: e.g., `impl<T: Bar, U, V: Baz> FooBar for Foo<T, U, V>
    // TODO where Self: BarBaz + BazBar { func foo(a: T, B: U) -> V }`
}
