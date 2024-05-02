use crate::{
    ast::{
        BlockExpr, Delimiter, FunctionItem, FunctionOrMethodParam, FunctionParam, Identifier,
        Keyword, OuterAttr, ReferenceOp, SelfParam, Type, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, test_utils::log_token, Parser};

impl FunctionItem {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<FunctionItem, ErrorsEmitted> {
        log_token(parser, "enter `FunctionItem::parse()`", true);

        let kw_func = parser.expect_keyword(TokenType::Func)?;

        let function_name = if let Some(Token::Identifier { name, .. }) = parser.current_token() {
            parser.next_token();
            log_token(parser, "consume token", false);

            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            log_token(parser, "consume token", false);
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let params = collection::get_collection_parens_comma(parser, FunctionOrMethodParam::parse)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            log_token(parser, "consume token", false);
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
            parser.next_token();
            log_token(parser, "consume token", false);

            let ty = Type::parse(parser)?;
            Some(Box::new(ty))
        } else {
            None
        };

        let block_opt = if let Some(Token::LBrace { .. }) = parser.current_token() {
            Some(BlockExpr::parse(parser)?)
        } else {
            None
        };

        log_token(parser, "exit `FunctionItem::parse()`", true);

        Ok(FunctionItem {
            attributes_opt,
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
        log_token(parser, "enter `FunctionOrMethodParam::parse()`", true);

        let token = parser.current_token();

        let prefix_opt = if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) = token {
            parser.next_token();
            log_token(parser, "consume token", false);

            match token {
                Some(Token::Ampersand { .. }) => Some(ReferenceOp::Borrow),
                Some(Token::AmpersandMut { .. }) => Some(ReferenceOp::MutableBorrow),
                _ => None,
            }
        } else {
            None
        };

        let param = match parser.current_token() {
            Some(Token::SelfKeyword { .. }) => {
                parser.next_token();
                log_token(parser, "consume token", false);

                let self_param = SelfParam {
                    prefix_opt,
                    kw_self: Keyword::SelfKeyword,
                };

                Ok(FunctionOrMethodParam::MethodParam(self_param))
            }
            Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
                let param_name = parser.get_identifier_patt()?;

                parser.expect_separator(TokenType::Colon)?;

                let param_type = Box::new(Type::parse(parser)?);

                let function_param = FunctionParam {
                    param_name,
                    param_type,
                };

                Ok(FunctionOrMethodParam::FunctionParam(function_param))
            }

            _ => {
                log_token(parser, "foo", true);

                parser.log_unexpected_str("`self` or identifier");
                Err(ErrorsEmitted)
            }
        };

        log_token(parser, "exit `FunctionOrMethodParam::parse()`", true);
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

        let statements = parser.parse();

        match statements {
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
