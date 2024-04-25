use crate::{
    ast::{
        BlockExpr, Delimiter, FunctionItem, FunctionOrMethodParam, FunctionParam, Identifier, Keyword, OuterAttr, SelfParam, Type, UnaryOp, Visibility
    },
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl FunctionItem {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<FunctionItem, ErrorsEmitted> {
        let kw_func = parser.expect_keyword(Token::Func {
            name: "func".to_string(),
            span: parser.stream.span(),
        })?;

        let mut params: Vec<FunctionOrMethodParam> = Vec::new();

        let function_name = if let Some(Token::Identifier { name, .. }) = parser.consume_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("identifier".to_string());
            Err(ErrorsEmitted)
        }?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`".to_string());
            Err(ErrorsEmitted)
        }?;

        // `&self` and `&mut self` can only occur as the first parameter in a method
        if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) = parser.peek_current() {
            let param = FunctionOrMethodParam::parse(parser)?;
            params.push(param);
        }

        loop {
            if let Some(Token::Comma { .. }) = parser.peek_current() {
                parser.consume_token();
            }

            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let param = FunctionOrMethodParam::parse(parser)?;
            params.push(param);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,
                Some(_) => parser.log_unexpected_token("`,` or `)`".to_string()),
                None => break,
            }
        }

        let close_paren = if let Some(Token::RParen { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_delimiter(')');
            Err(ErrorsEmitted)
        }?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Box::new(Type::parse(parser)?))
        } else {
            None
        };

        let block_opt = if let Some(Token::LBrace { .. }) = parser.peek_current() {
            Some(BlockExpr::parse(parser)?)
        } else if let Some(Token::Semicolon { .. }) = parser.peek_current() {
            parser.consume_token();
            None
        } else {
            None
        };

        Ok(FunctionItem {
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
        let prefix_opt = if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) =
            parser.peek_current()
        {
            parser.consume_token();
            Some(UnaryOp::Reference)
        } else {
            None
        };

        let token = parser.peek_current();

        let param = if let Some(Token::SelfKeyword { .. }) = token {
            let self_param = SelfParam {
                prefix_opt,
                kw_self: Keyword::SelfKeyword,
            };

            parser.consume_token();

            Ok(FunctionOrMethodParam::MethodParam(self_param))
        } else if let Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) = token
        {
            let param_name = parser.get_identifier_patt()?;

            let _ = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            })?;

            let param_type = Box::new(Type::parse(parser)?);

            let function_param = FunctionParam {
                param_name,
                param_type,
            };

            Ok(FunctionOrMethodParam::FunctionParam(function_param))
        } else {
            parser.log_unexpected_token("`self` or identifier".to_string());
            Err(ErrorsEmitted)
        };

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
        pub func only_owner(&mut self, mut caller: h160, ref balances: Mapping<u160, u256>)"#;

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
            let arr: [u64; 4] = [10, 20, 30, 40];
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
