use crate::{
    ast::{
        BlockExpr, Delimiter, FunctionItem, FunctionOrMethodParam, FunctionParam, Identifier,
        IdentifierPatt, Keyword, OuterAttr, ReferenceOp, SelfParam, Type, Visibility,
    },
    error::ErrorsEmitted,
    parser::ParseConstruct,
    token::Token,
};

use super::{collection, ParseDefinition, Parser};

impl ParseDefinition for FunctionItem {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<FunctionItem, ErrorsEmitted> {
        let kw_func = if let Some(Token::Func { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Func)
        } else {
            parser.log_unexpected_token("`func`");
            Err(ErrorsEmitted)
        }?;

        let function_name = match parser.next_token() {
            Some(Token::Identifier { name, .. }) => Ok(Identifier(name)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("function identifier");
                Err(ErrorsEmitted)
            }
        }?;

        let open_paren = match parser.next_token() {
            Some(Token::LParen { .. }) => Ok(Delimiter::LParen),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`(`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let params_opt =
            collection::get_collection(parser, FunctionOrMethodParam::parse, Delimiter::RParen)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_token("`)`");
            parser.log_unmatched_delimiter(&open_paren);
            Err(ErrorsEmitted)
        }?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
            parser.next_token();

            if parser.current_token().is_some() {
                Ok(Some(Box::new(Type::parse(parser)?)))
            } else {
                parser.log_missing("type", "function return type");
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        let block_opt = if let Some(Token::LBrace { .. }) = parser.current_token() {
            match parser.peek_ahead_by(1) {
                Some(Token::RBrace { .. }) => {
                    parser.next_token();
                    parser.next_token();
                    Ok(None)
                }
                Some(Token::EOF) | None => {
                    parser.log_missing_token("`}`");
                    parser.log_unmatched_delimiter(&open_paren);
                    Err(ErrorsEmitted)
                }
                _ => Ok(Some(BlockExpr::parse(parser)?)),
            }
        } else {
            Ok(None)
        }?;

        Ok(FunctionItem {
            attributes_opt,
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
        let token = parser.current_token();

        let prefix_opt = if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) = token {
            parser.next_token();

            match token {
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
                    prefix_opt,
                    kw_self: Keyword::SelfKeyword,
                };

                Ok(FunctionOrMethodParam::MethodParam(self_param))
            }
            Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
                let param_name = IdentifierPatt::parse(parser)?;

                match parser.current_token() {
                    Some(Token::Colon { .. }) => {
                        parser.next_token();
                    }
                    Some(Token::EOF) | None => {
                        parser.log_missing_token("`:`");
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
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

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
