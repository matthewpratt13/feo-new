use crate::{
    ast::{
        AliasDecl, ConstantDecl, FunctionOrMethodParam, Identifier, MethodSig, OuterAttr, TraitDef,
        TraitItem, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{
    item::{ParseDeclaration, ParseDefinition},
    Parser,
};

impl ParseDefinition for TraitDef {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<TraitDef, ErrorsEmitted> {
        let kw_trait = parser.expect_keyword(Token::Trait {
            name: "trait".to_string(),
            span: parser.stream.span(),
        })?;

        let mut associated_items: Vec<TraitItem> = Vec::new();

        let token = parser.consume_token();

        let trait_name = if let Some(Token::Identifier { name, .. }) = token {
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

            while let Some(oa) = parser.get_outer_attr() {
                item_attributes.push(oa);
                parser.consume_token();
            }

            let item_visibility = parser.get_visibility()?;

            let token = parser.peek_current();

            let associated_item = if let Some(Token::Const { .. }) = token {
                Ok(TraitItem::ConstantDecl(ConstantDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else if let Some(Token::Alias { .. }) = token {
                Ok(TraitItem::AliasDecl(AliasDecl::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else if let Some(Token::Func { .. }) = token {
                Ok(TraitItem::MethodSig(MethodSig::parse(
                    parser,
                    item_attributes,
                    item_visibility,
                )?))
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`const`, `alias` or `func`".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }?;

            associated_items.push(associated_item);

            // let token = parser.peek_current();

            // match token {
            //     Some(Token::Semicolon { .. }) => {
            //         parser.consume_token();
            //         continue;
            //     }
            //     Some(Token::RParen { .. }) => break,
            //     Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
            //         expected: "`;` or `)`".to_string(),
            //         found: Some(t),
            //     }),
            //     None => {
            //         parser.log_error(ParserErrorKind::MissingDelimiter { delim: ')' });
            //     }
            // }
        }

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(TraitDef {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_trait,
            trait_name,
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

impl MethodSig {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<MethodSig, ErrorsEmitted> {
        println!("ENTER `MethodSig::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let kw_func = parser.expect_keyword(Token::Func {
            name: "func".to_string(),
            span: parser.stream.span(),
        })?;

        let mut params: Vec<FunctionOrMethodParam> = Vec::new();

        let token = parser.consume_token();

        let function_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        }?;

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        // `&self` and `&mut self` can only occur as the first parameter in a method
        if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) = parser.peek_current() {
            let param = FunctionOrMethodParam::parse(parser)?;
            params.push(param);
        }

        println!("ENTER LOOP");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        loop {
            if let Some(Token::Comma { .. }) = parser.peek_current() {
                parser.consume_token();
            }

            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            println!("LOOP ITERATION");
            println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

            let param = FunctionOrMethodParam::parse(parser)?;
            params.push(param);

            let token = parser.peek_current();

            match token {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,
                Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `)`".to_string(),
                    found: Some(t),
                }),
                None => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: ')' });
                }
            }
        }

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Box::new(parser.get_type()?))
        } else {
            None
        };

        let semicolon = parser.expect_separator(Token::Semicolon {
            punc: ';',
            span: parser.stream.span(),
        })?;

        println!("EXIT `MethodSig::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(MethodSig {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_func,
            function_name,
            open_paren,
            params_opt: {
                if params.is_empty() {
                    None
                } else {
                    Some(params)
                }
            },
            close_paren,
            return_type_opt,
            semicolon,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_trait_def() -> Result<(), ()> {
        let input = r#"
        #[interface]
        pub trait Foo {
            #[storage]
            pub const OWNER: h160 = 0x12345123451234512345;
            pub alias NewType;

            #[modifier]
            func only_owner(&mut self, caller: h160);
            func transfer(&mut self, to: h160, amount: u256) -> Error;
            #[view]
            func sender(&self) -> h160;
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
