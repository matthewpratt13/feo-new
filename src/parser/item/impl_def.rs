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
    log_warn,
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
            parser.emit_unexpected_token(&TokenType::Impl.to_string());
            Err(ErrorsEmitted)
        }?;

        let generic_declaration_opt = parse_generic_params(parser)?;

        let token = parser.current_token().cloned();

        let nominal_type = match &token {
            Some(Token::Identifier { .. } | Token::SelfType { .. }) => {
                TypePath::parse(parser, token)
            }
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.warn_missing_token("implementing object type path");
                Err(ErrorsEmitted)
            }

            _ => {
                parser.emit_unexpected_token("implementing object type path");
                parser.next_token();
                Err(ErrorsEmitted)
            }
        }?;

        let generic_params_opt = match (generic_declaration_opt, parse_generic_params(parser)?) {
            (None, None) => None,
            (None, Some(ga)) => {
                parser.emit_error(ParserErrorKind::UndeclaredGenericParams {
                    found: format!("{:?}", ga.params),
                });

                return Err(ErrorsEmitted);
            }
            (Some(ga), None) => {
                log_warn!(parser.logger, "unused generics declared: {:?}", ga.params);
                None
            }
            (Some(_), Some(ga)) => Some(ga),
        };

        parser.expect_open_brace()?;

        let associated_items_opt = collection::get_associated_items::<InherentImplItem>(parser)?;

        let span = parser.get_braced_item_span(first_token.as_ref())?;

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
            parser.emit_unexpected_token(&TokenType::Impl.to_string());
            Err(ErrorsEmitted)
        }?;

        let generic_declaration_opt = parse_generic_params(parser)?;

        let token = parser.current_token().cloned();

        let implemented_trait_path = match &token {
            Some(Token::Identifier { .. }) => TypePath::parse(parser, token),
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.warn_missing_token("implemented trait path");
                Err(ErrorsEmitted)
            }

            _ => {
                parser.emit_unexpected_token("implemented trait path");
                parser.next_token();
                Err(ErrorsEmitted)
            }
        }?;

        let implemented_trait_generic_params_opt =
            match (generic_declaration_opt, parse_generic_params(parser)?) {
                (None, None) => None,
                (None, Some(ga)) => {
                    parser.emit_error(ParserErrorKind::UndeclaredGenericParams {
                        found: format!("{:?}", ga.params),
                    });

                    return Err(ErrorsEmitted);
                }
                (Some(ga), None) => {
                    log_warn!(parser.logger, "unused generics declared: {:?}", ga.params);
                    None
                }
                (Some(_), Some(ga)) => Some(ga),
            };

        let kw_for = parser
            .expect_token(TokenType::For)
            .and_then(|_| Ok(Keyword::For))?;

        let implementing_type = match parser.current_token() {
            Some(Token::Identifier { .. } | Token::SelfType { .. }) => Type::parse(parser),
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.warn_missing_token("implementing type");
                Err(ErrorsEmitted)
            }

            _ => {
                parser.emit_unexpected_token("implementing type");
                parser.next_token();
                Err(ErrorsEmitted)
            }
        }?;

        let implementing_type_generic_params_opt = parse_generic_params(parser)?;

        let where_clause_opt = parse_where_clause(parser)?;

        parser.expect_open_brace()?;

        let associated_items_opt = collection::get_associated_items::<TraitImplItem>(parser)?;

        let span = parser.get_braced_item_span(first_token.as_ref())?;

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
                    log_warn!(parser.logger, "assigned value cannot be `None`");
                    Err(ErrorsEmitted)
                } else {
                    Ok(InherentImplItem::ConstantDecl(constant_decl))
                }
            }

            Some(Token::Func { .. }) => {
                let function_def = FunctionItem::parse(parser, attributes_opt, visibility)?;
                if function_def.block_opt.is_none() {
                    parser.emit_missing_node("item", "function implementation");
                    parser.next_token();
                    Err(ErrorsEmitted)
                } else {
                    Ok(InherentImplItem::FunctionItem(function_def))
                }
            }
            _ => {
                parser.emit_unexpected_token(&format!(
                    "{} or {}",
                    TokenType::Const,
                    TokenType::Func
                ));
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
                    log_warn!(parser.logger, "assigned value cannot be `None`");
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
                    parser.emit_missing_node("item", "trait implementation associated item");
                    parser.next_token();
                    Err(ErrorsEmitted)
                } else {
                    Ok(TraitImplItem::FunctionItem(function_def))
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // TODO: add test for implementation def with generics and where clauses
    // TODO: e.g., `impl<T: Bar, U, V: Baz> Foo<T, U, V> { func foo(a: T, B: U) -> V }`
    // TODO: e.g., `impl<T: Bar, U, V: Baz> FooBar for Foo<T, U, V>
    // TODO where Self: BarBaz + BazBar { func foo(a: T, B: U) -> V }`
}
