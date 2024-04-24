use crate::{
    ast::{
        AliasDecl, ConstantDecl, Delimiter, FunctionItem, Identifier, InherentImplDef,
        InherentImplItem, OuterAttr, PathExpr, PathPrefix, TraitImplDef, TraitImplItem, Visibility,
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

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted(()))
        }?;

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
                Ok(InherentImplItem::FunctionDef(FunctionItem::parse(
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

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

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
        let kw_impl = parser.expect_keyword(Token::Impl {
            name: "impl".to_string(),
            span: parser.stream.span(),
        })?;

        let token = parser.consume_token();

        let implemented_trait_path = if let Some(Token::Identifier { name, .. }) = token {
            PathExpr::parse(parser, PathPrefix::Identifier(Identifier(name)))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "implemented trait path".to_string(),
                found: token,
            });

            Err(ErrorsEmitted(()))
        }?;

        let kw_for = parser.expect_keyword(Token::For {
            name: "for".to_string(),
            span: parser.stream.span(),
        })?;

        let implementing_type = parser.get_type()?;

        let mut associated_items: Vec<TraitImplItem> = Vec::new();

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted(()))
        }?;

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
                Ok(TraitImplItem::ConstantDecl(ConstantDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else if let Some(Token::Alias { .. }) = token {
                Ok(TraitImplItem::AliasDecl(AliasDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else if let Some(Token::Func { .. }) = token {
                Ok(TraitImplItem::FunctionDef(FunctionItem::parse(
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

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

        Ok(TraitImplDef {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            kw_impl,
            implemented_trait_path,
            kw_for,
            implementing_type,
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

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

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

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
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

                Event { msg: "transfer", to, amount }
            }
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
