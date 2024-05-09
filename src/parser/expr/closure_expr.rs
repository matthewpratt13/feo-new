use crate::{
    ast::{
        BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Delimiter, Expression, IdentifierPatt,
        Type,
    },
    error::ErrorsEmitted,
    parser::{collection, ParseConstruct, Parser, Precedence},
    token::Token,
};

impl ParseConstruct for ClosureExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let params = match parser.current_token() {
            Some(Token::Pipe { .. }) => {
                parser.next_token();

                let vec = collection::get_collection(parser, parse_closure_param, Delimiter::Pipe)?;

                if vec.is_some() {
                    Ok(ClosureParams::Some(vec.unwrap()))
                } else {
                    // TODO: replace with `MissingExpression`
                    parser.log_unexpected_token("closure parameters");
                    Err(ErrorsEmitted)
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

            if parser.current_token().is_some() {
                Ok(Some(Box::new(Type::parse(parser)?)))
            } else {
                parser.log_missing_token("return type");
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        let expression = if return_type_opt.is_some() {
            BlockExpr::parse(parser)?
        } else {
            parser.parse_expression(Precedence::Lowest)?
        };

        let expr = ClosureExpr {
            params,
            return_type_opt,
            expression: Box::new(expression),
        };

        Ok(Expression::Closure(expr))
    }
}

fn parse_closure_param(parser: &mut Parser) -> Result<ClosureParam, ErrorsEmitted> {
    let param_name = match parser.current_token() {
        Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
            IdentifierPatt::parse(parser)
        }

        // TODO: handle `None` case (`UnexpectedEndOfInput`)
        _ => {
            parser.log_unexpected_token("identifier");
            Err(ErrorsEmitted)
        }
    }?;

    let type_ann_opt = if let Some(Token::Colon { .. }) = parser.current_token() {
        parser.next_token();

        if parser.current_token().is_some() {
            Ok(Some(Type::parse(parser)?))
        } else {
            parser.log_missing_token("type");
            Err(ErrorsEmitted)
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
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_closure_expr_with_block() -> Result<(), ()> {
        let input = r#"|world: str| -> () {
            print("hello {}", world);
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}