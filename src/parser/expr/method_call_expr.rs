use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, Identifier, MethodCallExpr},
    error::ErrorsEmitted,
    parser::{collection, ParseOperatorExpr, Parser, Precedence},
    token::Token,
};

impl ParseOperatorExpr for MethodCallExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let receiver: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let method_name = match &parser.current_token().cloned() {
            Some(Token::Identifier { name, .. }) => {
                parser.next_token();
                Ok(Identifier::from(name))
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

        let open_paren = match &parser.current_token() {
            Some(Token::LParen { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LParen { position })
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let args_opt = collection::get_expressions(parser, Precedence::Lowest, &open_paren)?;

        match &parser.current_token() {
            Some(Token::RParen { .. }) => {
                parser.next_token();

                let expr = MethodCallExpr {
                    receiver: Box::new(receiver),
                    method_name,
                    args_opt,
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
