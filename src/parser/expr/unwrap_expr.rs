use crate::{
    ast::{Expression, UnwrapExpr, UnwrapOp, ValueExpr},
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    token::Token,
};

impl ParseOperatorExpr for UnwrapExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let value_expr = Box::new(ValueExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?);

        match parser.current_token() {
            Some(Token::QuestionMark { .. }) => {
                parser.next_token();
                let expr = UnwrapExpr {
                    value_expr,
                    unwrap_op: UnwrapOp,
                };

                Ok(Expression::Unwrap(expr))
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("unwrap operator (`?`)");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("unwrap operator (`?`)");
                Err(ErrorsEmitted)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_unwrap_expr() -> Result<(), ()> {
        let input = r#"foo?"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
