use core::fmt;

use crate::{
    ast::{Delimiter, Identifier, InnerAttr, Item, Keyword, ModuleItem, OuterAttr, Visibility},
    error::ErrorsEmitted,
    span::Position,
    token::Token,
};

use super::{collection, ParseDefItem, Parser};

impl ParseDefItem for ModuleItem {
    fn parse(
        parser: &mut Parser,
        outer_attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<ModuleItem, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_module = if let Some(Token::Module { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Module)
        } else {
            parser.log_unexpected_token("`module`");
            Err(ErrorsEmitted)
        }?;

        let module_name = match parser.next_token() {
            Some(Token::Identifier { name, .. }) => Ok(Identifier::from(&name)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("module identifier");
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

        let (inner_attributes_opt, items_opt) = parse_items(parser)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());

                parser.next_token();

                Ok(ModuleItem {
                    outer_attributes_opt,
                    visibility,
                    kw_module,
                    module_name,
                    inner_attributes_opt,
                    items_opt,
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

impl fmt::Debug for ModuleItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleItem")
            .field("outer_attributes_opt", &self.outer_attributes_opt)
            .field("visibility", &self.visibility)
            .field("module_name", &self.module_name)
            .field("inner_attributes_opt", &self.inner_attributes_opt)
            .field("items_opt", &self.items_opt)
            .finish()
    }
}

fn parse_items(
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

            import lib::foo_error::Error;
           
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

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
