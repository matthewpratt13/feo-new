use crate::{
    ast::{Delimiter, Identifier, InnerAttr, Item, Keyword, ModuleItem, OuterAttr, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use super::{collection, ParseDefinition, Parser};

impl ParseDefinition for ModuleItem {
    fn parse(
        parser: &mut Parser,
        outer_attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<ModuleItem, ErrorsEmitted> {
        let kw_module = if let Some(Token::Module { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Module)
        } else {
            parser.log_unexpected_token("`module`");
            Err(ErrorsEmitted)
        }?;

        let module_name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
            // TODO: handle `None` case (`UnexpectedEndOfInput`)
        } else {
            parser.log_unexpected_token("module name");
            Err(ErrorsEmitted)
        }?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
            Ok(Delimiter::LBrace)
            // TODO: handle `None` case (`MissingToken`)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let (inner_attributes_opt, items_opt) = parse_item(parser)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(&open_brace);
            Err(ErrorsEmitted)
        }?;

        Ok(ModuleItem {
            outer_attributes_opt,
            visibility,
            kw_module,
            module_name,
            open_brace,
            inner_attributes_opt,
            items_opt,
            close_brace,
        })
    }
}

fn parse_item(
    parser: &mut Parser,
) -> Result<(Option<Vec<InnerAttr>>, Option<Vec<Item>>), ErrorsEmitted> {
    let inner_attributes_opt = collection::get_attributes(parser, InnerAttr::inner_attr);

    let mut items: Vec<Item> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let attributes_opt = collection::get_attributes::<OuterAttr>(parser, OuterAttr::outer_attr);

        let item_visibility = Visibility::visibility(parser)?;

        let item = Item::parse(parser, attributes_opt, item_visibility)?;
        items.push(item);
    }

    let items_opt = match items.is_empty() {
        true => Ok(None),
        false => Ok(Some(items)),
    }?;

    Ok((inner_attributes_opt, items_opt))
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_module_def() -> Result<(), ()> {
        let input = r#"
        pub module foo {
            #![contract]

            import package::foo_error::Error;
           
            trait Bar {
                #![interface]

                const ADDRESS: h160;
                func transfer(&mut self, to: h160, amount: u256) -> Result<Baz, Error> {}
            }

            trait FooBar {
                #[modifier]
                func only_owner(caller: h160) -> bool {}
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

            impl FooBar for Foo {
                func only_owner(caller: h160) -> bool {
                    if (caller != OWNER) {
                        return true;
                    }
                }
            }
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
