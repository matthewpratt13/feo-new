use super::{collection, parse_generic_params, ParseDefItem, Parser};

use crate::{
    ast::{
        BlockExpr, FunctionItem, FunctionOrMethodParam, FunctionParam, IdentifierPatt, Keyword,
        OuterAttr, ReferenceOp, SelfParam, Type, Visibility,
    },
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, ParsePattern},
    token::{Token, TokenType},
};

use core::fmt;

impl ParseDefItem for FunctionItem {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<FunctionItem, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_func = if let Some(Token::Func { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Func)
        } else {
            parser.log_unexpected_token("`func`");
            Err(ErrorsEmitted)
        }?;

        let function_name = parser.expect_identifier()?;

        let generic_params_opt = parse_generic_params(parser)?;

        let open_paren = parser.expect_delimiter(TokenType::LParen).and_then(|d| {
            d.ok_or_else(|| {
                parser.logger.warn(&format!(
                    "bad input to `Parser::expect_delimiter()` function. Expected delimiter token, found {:?}",
                    parser.current_token()
                ));
                ErrorsEmitted
            })
        })?;

        let params_opt =
            collection::get_collection(parser, FunctionOrMethodParam::parse, &open_paren)?;

        parser.expect_closing_paren(&open_paren)?;

        let mut last_token = parser.current_token().cloned();

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
            parser.next_token();

            let token = parser.current_token();

            if token.is_some() {
                last_token = token.cloned();
                Ok(Some(Box::new(Type::parse(parser)?)))
            } else {
                parser.log_missing("type", "function return type");
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        let block_opt = if let Some(Token::LBrace { .. }) = parser.current_token().cloned() {
            match parser.peek_ahead_by(1) {
                Some(Token::RBrace { .. }) => {
                    parser.next_token();

                    last_token = parser.current_token().cloned();

                    parser.next_token();

                    Ok(None)
                }
                Some(Token::EOF) | None => {
                    parser.log_unmatched_delimiter(&open_paren); // TODO: should be `{`
                    parser.log_missing_token("`}`");
                    Err(ErrorsEmitted)
                }
                _ => Ok(Some(BlockExpr::parse(parser)?)),
            }
        } else {
            Ok(None)
        }?;

        if let Some(Token::Semicolon { .. }) = parser.current_token() {
            parser.next_token();
        }

        let span = parser.get_span(&first_token.unwrap().span(), &last_token.unwrap().span());

        Ok(FunctionItem {
            attributes_opt,
            visibility,
            kw_func,
            function_name,
            generic_params_opt,
            params_opt,
            return_type_opt,
            block_opt,
            span,
        })
    }
}

impl fmt::Debug for FunctionItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionItem")
            .field("attributes_opt", &self.attributes_opt)
            .field("visibility", &self.visibility)
            .field("function_name", &self.function_name)
            .field("params_opt", &self.params_opt)
            .field("return_type_opt", &self.return_type_opt)
            .field("block_opt", &self.block_opt)
            .finish()
    }
}

impl FunctionOrMethodParam {
    pub(crate) fn parse(parser: &mut Parser) -> Result<FunctionOrMethodParam, ErrorsEmitted> {
        let reference_op_opt = if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) =
            parser.current_token()
        {
            parser.next_token();

            match parser.current_token() {
                Some(Token::Ampersand { .. }) => Some(ReferenceOp::Borrow),
                Some(Token::AmpersandMut { .. }) => Some(ReferenceOp::MutableBorrow),
                _ => None,
            }
        } else {
            None
        };

        match parser.current_token() {
            Some(Token::SelfKeyword { .. }) => {
                parser.next_token();

                let self_param = SelfParam {
                    reference_op_opt,
                    kw_self: Keyword::SelfKeyword,
                };

                Ok(FunctionOrMethodParam::MethodParam(self_param))
            }
            Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
                let param_name = IdentifierPatt::parse_patt(parser)?;

                match parser.current_token() {
                    Some(Token::Colon { .. }) => {
                        parser.next_token();
                    }

                    Some(Token::RParen { .. } | Token::Comma { .. }) => {
                        parser.log_missing("type", "function parameter type annotation");
                        return Err(ErrorsEmitted);
                    }

                    Some(Token::EOF) | None => {
                        parser.log_unexpected_eoi();
                        return Err(ErrorsEmitted);
                    }
                    _ => {
                        parser.log_unexpected_token("`:`");
                        return Err(ErrorsEmitted);
                    }
                }

                let param_type = Box::new(Type::parse(parser)?);

                let function_param = FunctionParam {
                    param_name,
                    param_type,
                };

                Ok(FunctionOrMethodParam::FunctionParam(function_param))
            }

            _ => {
                parser.log_unexpected_token("function or method parameter (identifier or `self`)");
                Err(ErrorsEmitted)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_function_def_without_block() -> Result<(), ()> {
        let input = r#"
        #[modifier]
        pub func only_owner(&mut self, mut caller: h160, ref balances: Mapping<u160, u256>)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_function_def_with_block() -> Result<(), ()> {
        let input = r#"
        func foo(bar: &mut str, baz: u64) -> CustomType {
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

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    // TODO: add test for function item with generics
    // TODO e.g., `func foo<T: Bar, U, V: Baz>(a: T, b: U) -> V { … }>
}
