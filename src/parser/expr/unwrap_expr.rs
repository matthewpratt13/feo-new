use crate::{
    ast::{Expression, UnwrapExpr, UnwrapOp, ValueExpr},
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    span::Spanned,
    token::Token,
};

use core::fmt;

impl ParseOperatorExpr for UnwrapExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let value_expr = Box::new(ValueExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?);

        let last_token = parser.current_token();

        match &last_token {
            Some(Token::QuestionMark { .. }) => {
                let span = parser.get_span(left_expr_span, &last_token.unwrap().span());

                parser.next_token();

                let expr = UnwrapExpr {
                    value_expr,
                    unwrap_op: UnwrapOp,
                    span,
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

impl fmt::Debug for UnwrapExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UnwrapExpr")
            .field("value_expr", &self.value_expr)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_unwrap_expr() -> Result<(), ()> {
        let input = r#"foo?"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
