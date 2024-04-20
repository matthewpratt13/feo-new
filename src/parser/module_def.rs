use crate::{
    ast::{
        AliasDecl, ConstantDecl, EnumDef, FunctionDef, Identifier, ImportDecl, InherentImplDef,
        InnerAttr, Item, ModuleDef, OuterAttr, StaticItemDecl, StructDef, TraitDef, TraitImplDef,
        Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{
    item::{ParseDeclaration, ParseDefinition},
    Parser,
};

impl ModuleDef {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<InnerAttr>,
        visibility: Visibility,
    ) -> Result<ModuleDef, ErrorsEmitted> {
        let kw_mod = parser.expect_keyword(Token::Mod {
            name: "mod".to_string(),
            span: parser.stream.span(),
        })?;

        let mut items: Vec<Item> = Vec::new();

        let token = parser.consume_token();

        let module_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });

            Err(ErrorsEmitted(()))
        }?;

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let mut item_attributes: Vec<OuterAttr> = Vec::new();

            let mut module_attributes: Vec<InnerAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                item_attributes.push(oa);
                parser.consume_token();
            }

            while let Some(ia) = parser.get_inner_attr() {
                module_attributes.push(ia);
                parser.consume_token();
            }

            let item_visibility = parser.get_visibility()?;

            let token = parser.peek_current();

            let item = match token {
                Some(Token::Import { .. }) => Ok(Item::ImportDecl(ImportDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?)),
                Some(Token::Alias { .. }) => Ok(Item::AliasDecl(AliasDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?)),
                Some(Token::Const { .. }) => Ok(Item::ConstantDecl(ConstantDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?)),
                Some(Token::Static { .. }) => Ok(Item::StaticItemDecl(StaticItemDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?)),
                Some(Token::Mod { .. }) => Ok(Item::ModuleDef(Box::new(ModuleDef::parse(
                    parser,
                    module_attributes,
                    item_visibility,
                )?))),
                Some(Token::Trait { .. }) => Ok(Item::TraitDef(TraitDef::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?)),
                Some(Token::Enum { .. }) => Ok(Item::EnumDef(EnumDef::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?)),
                Some(Token::Struct { .. }) => Ok(Item::StructDef(StructDef::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?)),
                Some(Token::Impl { .. }) => {
                    if let Some(Token::For { .. }) = parser.peek_ahead_by(2) {
                        Ok(Item::TraitImplDef(TraitImplDef::parse(
                            parser,
                            item_attributes,
                            item_visibility,
                        )?))
                    } else {
                        Ok(Item::InherentImplDef(InherentImplDef::parse(
                            parser,
                            item_attributes,
                            item_visibility,
                        )?))
                    }
                }
                Some(Token::Func { .. }) => Ok(Item::FunctionDef(FunctionDef::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?)),
                _ => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "declaration or definition".to_string(),
                        found: token,
                    });
                    Err(ErrorsEmitted(()))
                }
            }?;

            items.push(item);
        }

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        Ok(ModuleDef {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_mod,
            module_name,
            open_brace,
            items_opt: {
                if items.is_empty() {
                    None
                } else {
                    Some(items)
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
    fn parse_module_def() -> Result<(), ()> {
        let input = r#"
        #![contract]
        mod foo {
            #[storage]
            static mut OWNER: h160 = 0x12345123451234512345;

            #[interface]
            pub trait Bar {
                func transfer(&mut self, to: h160, amount: u256) -> Baz;
            }

            #[event]
            pub struct Baz {
                #[topic]
                to: h160,
                amount: u256,
            }

            pub struct Foo {
                owner: h160,
                balance: u256,
            }

            impl Foo {
                #[modifier]
                pub func only_owner(caller: h160) -> bool {
                    if (caller != OWNER) {
                        return true
                    }
                }

                #[constructor]
                pub func new(address: h160, amount: u256) -> Foo {
                    Foo { owner: address, balance: amount }
                }
            }

            impl Bar for Foo {
                func transfer(&mut self, to: h160, amount: u256) -> Baz {
                    self.balance -= amount;
                    to.balance += amount;

                    Baz { to: to, amount: amount }
                }
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
