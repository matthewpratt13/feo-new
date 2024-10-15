use core::fmt;

use crate::{
    ast::{
        BlockExpr, Delimiter, FunctionItem, FunctionOrMethodParam, FunctionParam, IdentifierPatt,
        Keyword, OuterAttr, ReferenceOp, SelfParam, Type, Visibility,
    },
    error::ErrorsEmitted,
    parser::{get_collection, ParseConstructExpr, ParseDefItem, ParsePattern, Parser},
    token::{Token, TokenType},
};

use super::parse_generic_params;

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
            parser.emit_unexpected_token(&TokenType::Func.to_string());
            Err(ErrorsEmitted)
        }?;

        let function_name = parser.expect_identifier("function name")?;

        let generic_params_opt = parse_generic_params(parser)?;

        let open_paren = parser.expect_open_paren()?;

        let params_opt = get_collection(parser, FunctionOrMethodParam::parse, &open_paren)?;

        parser.expect_closing_paren()?;

        let mut last_token = parser.current_token().cloned();

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
            parser.next_token();

            let token = parser.current_token();

            if token.is_some() {
                last_token = token.cloned();
                Ok(Some(Box::new(Type::parse(parser)?)))
            } else {
                parser.emit_missing_node("type", "function return type");
                parser.next_token();
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
                    let position = parser.current_position();
                    parser.emit_unexpected_eoi();
                    parser.warn_unmatched_delimiter(&Delimiter::LBrace { position });
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
            match parser.current_token() {
                Some(Token::Ampersand { .. }) => {
                    parser.next_token();

                    Some(ReferenceOp::Borrow)
                }
                Some(Token::AmpersandMut { .. }) => {
                    parser.next_token();

                    Some(ReferenceOp::MutableBorrow)
                }
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

                parser.expect_token(TokenType::Colon)?;

                let param_type = match parser.current_token() {
                    Some(Token::Comma { .. } | Token::RParen { .. }) => {
                        parser.emit_missing_node("type", "function parameter type");
                        parser.next_token();
                        Err(ErrorsEmitted)
                    }
                    Some(Token::EOF) | None => {
                        parser.emit_unexpected_eoi();
                        parser.emit_missing_node("type", "function parameter type");
                        Err(ErrorsEmitted)
                    }
                    _ => Type::parse(parser),
                }?;

                let function_param = FunctionParam {
                    param_name,
                    param_type: Box::new(param_type),
                };

                Ok(FunctionOrMethodParam::FunctionParam(function_param))
            }

            _ => {
                parser.emit_unexpected_token("function or method parameter (identifier or `self`)");
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    // TODO: add test for function item with generics
    // TODO e.g., `func foo<T: Bar, U, V: Baz>(a: T, b: U) -> V { … }>
}
