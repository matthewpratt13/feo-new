use crate::{
    ast::{
        BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Delimiter, Expression, IdentifierPatt,
        Type,
    },
    error::ErrorsEmitted,
    parser::{collection, ParseConstructExpr, Parser, Precedence},
    token::Token,
};

impl ParseConstructExpr for ClosureExpr {
    fn parse(parser: &mut Parser) -> Result<ClosureExpr, ErrorsEmitted> {
        let closure_params = match parser.current_token() {
            Some(Token::Pipe { .. }) => {
                let position = parser.current_position();
                let open_pipe = Delimiter::Pipe { position };
                parser.next_token();

                let vec_opt = collection::get_collection(parser, parse_closure_param, &open_pipe)?;

                match parser.current_token() {
                    Some(Token::Pipe { .. }) => {
                        if vec_opt.is_some() {
                            parser.next_token();
                            Ok(ClosureParams::Some(vec_opt.unwrap()))
                        } else {
                            parser.log_missing("patt", "closure parameters");
                            Err(ErrorsEmitted)
                        }
                    }
                    _ => {
                        parser.log_unmatched_delimiter(&open_pipe);
                        Err(ErrorsEmitted)
                    }
                }
            }
            Some(Token::DblPipe { .. }) => {
                parser.next_token();
                Ok(ClosureParams::None)
            }
            _ => {
                parser.log_unexpected_token("`|` or `||`");
                Err(ErrorsEmitted)
            }
        }?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
            parser.next_token();

            match parser.current_token() {
                Some(Token::LBrace { .. }) => {
                    parser.log_missing("type", "closure return type");
                    Err(ErrorsEmitted)
                }
                Some(Token::EOF { .. }) | None => {
                    parser.log_unexpected_eoi();
                    Err(ErrorsEmitted)
                }

                _ => Ok(Some(Box::new(Type::parse(parser)?))),
            }
        } else {
            Ok(None)
        }?;

        let body_expression = if return_type_opt.is_some() {
            Box::new(Expression::Block(BlockExpr::parse(parser)?))
        } else {
            Box::new(parser.parse_expression(Precedence::Lowest)?)
        };

        let expr = ClosureExpr {
            closure_params,
            return_type_opt,
            body_expression,
        };

        Ok(expr)
    }
}

fn parse_closure_param(parser: &mut Parser) -> Result<ClosureParam, ErrorsEmitted> {
    let position = parser.current_position();
    let open_pipe = Delimiter::Pipe { position };

    let param_name = match parser.current_token() {
        Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
            IdentifierPatt::parse(parser)
        }

        Some(Token::EOF) | None => {
            parser.log_missing("patt", "identifier pattern");
            Err(ErrorsEmitted)
        }

        _ => {
            parser.log_unexpected_token("identifier, `ref` or `mut`");
            Err(ErrorsEmitted)
        }
    }?;

    let type_ann_opt = if let Some(Token::Colon { .. }) = parser.current_token() {
        parser.next_token();

        match parser.current_token() {
            Some(Token::Comma { .. } | Token::Pipe { .. }) => {
                parser.log_missing("type", "closure parameter type annotation");
                Err(ErrorsEmitted)
            }
            Some(Token::EOF { .. }) | None => {
                parser.log_unmatched_delimiter(&open_pipe);
                Err(ErrorsEmitted)
            }

            _ => Ok(Some(Box::new(Type::parse(parser)?))),
        }
    } else {
        Ok(None)
    }?;

    let param = ClosureParam {
        param_name,
        type_ann_opt,
    };

    Ok(param)
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_closure_expr_without_block() -> Result<(), ()> {
        let input = r#"|| 2 + 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_closure_expr_with_block() -> Result<(), ()> {
        let input = r#"|world: str| -> () {
            print("hello {}", world)
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
