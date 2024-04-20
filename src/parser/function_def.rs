use crate::{
    ast::{
        BlockExpr, FunctionDef, FunctionOrMethodParam, FunctionParam, Identifier, Keyword,
        OuterAttr, SelfParam, UnaryOp, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl FunctionDef {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<FunctionDef, ErrorsEmitted> {
        println!("ENTER `FunctionDef::parse()`");
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
            parser.consume_token();
        }

        println!("ENTER LOOP");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        loop {
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

        let block_opt = if let Some(Token::LBrace { .. }) = parser.peek_current() {
            Some(BlockExpr::parse(parser)?)
        } else {
            None
        };

        println!("EXIT `FunctionDef::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(FunctionDef {
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
            block_opt,
        })
    }
}

impl FunctionOrMethodParam {
    pub(crate) fn parse(parser: &mut Parser) -> Result<FunctionOrMethodParam, ErrorsEmitted> {
        println!("ENTER `FunctionOrMethodParam::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let prefix_opt = if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) =
            parser.peek_current()
        {
            parser.consume_token();
            Some(UnaryOp::Reference)
        } else {
            None
        };

        let token = parser.consume_token();

        println!("OPTIONAL PREFIX: {:?}", prefix_opt);
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let param = if let Some(Token::SelfKeyword { .. }) = token {
            let self_param = SelfParam {
                prefix_opt,
                kw_self: Keyword::SelfKeyword,
            };

            Ok(FunctionOrMethodParam::MethodParam(self_param))
        } else if let Some(Token::Identifier { name, .. }) = token {
            let _ = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            })?;

            let param_type = Box::new(parser.get_type()?);

            let function_param = FunctionParam {
                param: Identifier(name),
                param_type,
            };

            Ok(FunctionOrMethodParam::FunctionParam(function_param))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`self` or identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        };

        println!("PARAM: {:?}", param);

        println!("EXIT `FunctionOrMethodParam::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        param
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_function_def_without_block() -> Result<(), ()> {
        let input = r#"
        #[modifier]
        pub func only_owner(&mut self, caller: h160)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_function_def_with_block() -> Result<(), ()> {
        let input = r#"
        pub func foo(bar: &mut str, baz: u64) -> CustomType {
            let arr = [10, 20, 30, 40];
            let mut counter = 0;
            
            if (baz < 30) {
                print("foobar");
            } else {
                bar.push("baz");
            }

            for element in arr {
                print("{}", element);
            }

            while (counter < bar.len()) {
                counter += 1;
            }

            return CustomType {
                param1: true,
                param2: x + 2,
                param3: b"foo",
            };
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
