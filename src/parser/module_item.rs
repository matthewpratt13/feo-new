use crate::{
    ast::{Delimiter, Identifier, InnerAttr, Item, ModuleItem, OuterAttr, Visibility},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::Parser;

impl ModuleItem {
    pub(crate) fn parse(
        parser: &mut Parser, 
        outer_attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<ModuleItem, ErrorsEmitted> {
        let kw_module = parser.expect_keyword(TokenType::Module)?;

        let token = parser.next_token();

        let module_name = if let Some(Token::Identifier { name, .. }) = token {
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

        let mut inner_attributes: Vec<InnerAttr> = Vec::new();

        while let Some(ia) = InnerAttr::inner_attr(parser) {
            inner_attributes.push(ia);
            parser.next_token();
        }

        let mut items: Vec<Item> = Vec::new();

        while !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            let mut item_attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = OuterAttr::outer_attr(parser) {
                item_attributes.push(oa);
                parser.next_token();
            }

            let item_visibility = Visibility::visibility(parser)?;

            let item = Item::parse(parser, item_attributes, item_visibility)?;
            items.push(item);
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

        Ok(ModuleItem {
            outer_attributes_opt: {
                if outer_attributes.is_empty() {
                    None
                } else {
                    Some(outer_attributes)
                }
            },
            visibility,
            kw_module,
            module_name,
            open_brace,
            inner_attributes_opt: {
                if inner_attributes.is_empty() {
                    None
                } else {
                    Some(inner_attributes)
                }
            },
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
        pub module foo {
            #![contract]

            import package::foo_error::Error;
         
            pub trait Bar {
                #![interface]

                func transfer(&mut self, to: h160, amount: u256) -> Result<Baz, Error>;
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
                #[storage]
                const ADDRESS: h160 = $0x12345123451234512345;
    
                #[modifier]
                pub func only_owner(caller: h160) -> bool {
                    if (caller != OWNER) {
                        return true;
                    }
                }

                #[constructor]
                pub func new(address: h160, amount: u256) -> Foo {
                    Foo { owner: address, balance: amount }
                }
            }

            impl Bar for Foo {
                func transfer(&mut self, to: h160, amount: u256) -> Result<Baz, Error> {
                    self.balance -= amount;
                    to.balance += amount;

                    Ok(Baz { to: to, amount: amount })
                }
            }
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
