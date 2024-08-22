use crate::{
    ast::{AssigneeExpr, Expression, Identifier, MethodCallExpr},
    error::ErrorsEmitted,
    parser::{collection, ParseOperatorExpr, Parser, Precedence},
    span::Spanned,
    token::{Token, TokenType},
};

use core::fmt;

impl ParseOperatorExpr for MethodCallExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let receiver: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let method_name = match parser.current_token().cloned() {
            Some(Token::Identifier { name, .. }) => {
                parser.next_token();
                Ok(Identifier::from(&name))
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("identifier");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("identifier");
                Err(ErrorsEmitted)
            }
        }?;

        let open_paren = parser.expect_delimiter(TokenType::LParen).and_then(|d| {
            d.ok_or_else(|| {
                parser.logger.warn(&format!(
                    "bad input to `Parser::expect_delimiter()` function. Expected delimiter token, found {:?}",
                    parser.current_token()
                ));
                ErrorsEmitted
            })
        })?;

        let args_opt = collection::get_expressions(parser, Precedence::Lowest, &open_paren)?;

        let last_token = parser.current_token();

        match &last_token {
            Some(Token::RParen { .. }) => {
                let span = parser.get_span(left_expr_span, &last_token.unwrap().span());

                parser.next_token();

                let expr = MethodCallExpr {
                    receiver: Box::new(receiver),
                    method_name,
                    args_opt,
                    span,
                };

                Ok(Expression::MethodCall(expr))
            }
            _ => {
                parser.log_unmatched_delimiter(&open_paren);
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for MethodCallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MethodCallExpr")
            .field("receiver", &self.receiver)
            .field("method_name", &self.method_name)
            .field("args_opt", &self.args_opt)
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
    fn parse_method_call_expr_with_args() -> Result<(), ()> {
        let input = r#"receiver.method(x, "foo", -10)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_method_call_expr_without_args() -> Result<(), ()> {
        let input = r#"receiver.method()"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
