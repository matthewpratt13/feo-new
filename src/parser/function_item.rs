use crate::{
    ast::{
        BlockExpr, Delimiter, FunctionItem, FunctionOrMethodParam, FunctionParam, Identifier,
        Keyword, OuterAttr, ReferenceOp, SelfParam, Type, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{test_utils::log_token, Parser};

impl FunctionItem {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<FunctionItem, ErrorsEmitted> {
        log_token(parser, "enter `FunctionItem::parse()`", true);

        let kw_func = parser.expect_keyword(TokenType::Func)?;

        let function_name = if let Some(Token::Identifier { name, .. }) = parser.peek_current() {
            parser.consume_token();
            log_token(parser, "consume token", false);

            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.peek_current() {
            parser.consume_token();
            log_token(parser, "consume token", false);
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        // Parse the function params
        let params_opt = parse_function_params(parser);

        let close_paren = if let Some(Token::RParen { .. }) = parser.consume_token() {
            log_token(parser, "consume token", false);
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

        // Parse the return type (optional)
        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.peek_current() {
            parser.consume_token(); // Consume the arrow '->'
            log_token(parser, "consume token", false);

            let ty = Type::parse(parser)?;
            Some(Box::new(ty)) // Parse the return type
        } else {
            None // No return type
        };

        // Check for the function body (optional)
        let block_opt = if let Some(Token::LBrace { .. }) = parser.peek_current() {
            Some(BlockExpr::parse(parser)?) // Parse the function body
        } else {
            None // No function body (function signature)
        };

        log_token(parser, "exit `FunctionItem::parse()`", true);

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
            params_opt,
            close_paren,
            return_type_opt,
            block_opt,
        })
    }
}

impl FunctionOrMethodParam {
    pub(crate) fn parse(parser: &mut Parser) -> Result<FunctionOrMethodParam, ErrorsEmitted> {
        log_token(parser, "enter `FunctionOrMethodParam::parse()`", true);

        let token = parser.peek_current();

        let prefix_opt = if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) = token {
            parser.consume_token();
            log_token(parser, "consume token", false);

            match token {
                Some(Token::Ampersand { .. }) => Some(ReferenceOp::Borrow),
                Some(Token::AmpersandMut { .. }) => Some(ReferenceOp::MutableBorrow),
                _ => None,
            }
        } else {
            None
        };

        // Parse individual function params
        let param = match parser.peek_current() {
            Some(Token::SelfKeyword { .. }) => {
                parser.consume_token();
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

                // Check for a type annotation
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

        // log_token(parser, "enter `FunctionOrMethodParam::parse()`", true);

        // let prefix_opt = if let Some(t) = parser.peek_current() {
        //     parser.consume_token();
        //     log_token(parser, "consume token", false);

        //     match t {
        //         Token::Ampersand { .. } => Some(ReferenceOp::Borrow),
        //         Token::AmpersandMut { .. } => Some(ReferenceOp::MutableBorrow),
        //         _ => None,
        //     }
        // } else {
        //     None
        // };

        // let token = parser.peek_current();

        // let param = if let Some(Token::parserKeyword { .. }) = token {
        //     let parser_param = parserParam {
        //         prefix_opt,
        //         kw_parser: Keyword::parserKeyword,
        //     };

        //     parser.consume_token();
        //     log_token(parser, "consume token", false);

        //     Ok(FunctionOrMethodParam::MethodParam(parser_param))
        // } else if let Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) = token
        // {
        //     let param_name = parser.get_identifier_patt()?;

        //     parser.expect_separator(TokenType::Colon)?;

        //     let param_type = Box::new(Type::parse(parser)?);

        //     let function_param = FunctionParam {
        //         param_name,
        //         param_type,
        //     };

        //     Ok(FunctionOrMethodParam::FunctionParam(function_param))
        // } else {
        //     parser.log_unexpected_str("`parser` or identifier");
        //     Err(ErrorsEmitted)
        // };

        // log_token(parser, "exit `FunctionOrMethodParam::parse()`", true);
        // param
    }
}

// fn parse_function_param(parser: &mut Parser) -> Result<FunctionOrMethodParam, ErrorsEmitted> {
// let prefix_opt = if let Some(t) = parser.peek_current() {
//     parser.consume_token();
//     log_token(parser, "consume token", false);

//     match t {
//         Token::Ampersand { .. } => Some(ReferenceOp::Borrow),
//         Token::AmpersandMut { .. } => Some(ReferenceOp::MutableBorrow),
//         _ => None,
//     }
// } else {
//     None
// };

// // Parse individual function params
// match parser.peek_current() {
//     Some(
//         Token::Identifier { name, .. } | Token::Ref { name, .. } | Token::Mut { name, .. },
//     ) => {
//         let param_name = parser.get_identifier_patt()?;
//         // parser.consume_token(); // Consume the identifier

//         parser.expect_separator(TokenType::Colon)?;

//         // Check for a type annotation
//         let param_type = Box::new(Type::parse(parser)?);

//         let param = FunctionParam {
//             param_name,
//             param_type,
//         };

//         Ok(FunctionOrMethodParam::FunctionParam(param))
//     }

//     Some(Token::SelfKeyword { .. }) => {
//         parser.consume_token();
//         let param = SelfParam {
//             prefix_opt,
//             kw_self: Keyword::SelfKeyword,
//         };

//         Ok(FunctionOrMethodParam::MethodParam(param))
//     }

//     _ => {
//         parser.log_unexpected_str("`self` or identifier");
//         Err(ErrorsEmitted)
//     }
// }
// }

fn parse_function_params(parser: &mut Parser) -> Option<Vec<FunctionOrMethodParam>> {
    let mut params = Vec::new();

    // Parse each param
    while !matches!(
        parser.peek_current(),
        Some(Token::RParen { .. } | Token::EOF)
    ) {
        if let Ok(param) = FunctionOrMethodParam::parse(parser) {
            params.push(param);

            if let Some(Token::Comma { .. }) = parser.peek_current() {
                parser.consume_token(); // Consume the comma separating params
            }
        }

        if matches!(parser.peek_current(), Some(Token::RParen { .. })) {
            break;
        }
    }

    if params.is_empty() {
        None
    } else {
        Some(params)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_function_def_without_block() -> Result<(), ()> {
        let input = r#"
        #[modifier]
        pub func only_owner(&mut parser, mut caller: h160, ref balances: Mapping<u160, u256>)"#;

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
