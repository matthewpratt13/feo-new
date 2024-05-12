use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, Identifier, MethodCallExpr},
    error::ErrorsEmitted,
    parser::{collection, ParseOperation, Parser, Precedence},
    token::Token,
};

impl ParseOperation for MethodCallExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let assignee_expr = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let method_name = match parser.current_token() {
            Some(Token::Identifier { name, .. }) => {
                parser.next_token();
                Ok(Identifier(name))
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

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let args_opt = collection::get_expressions(parser, Precedence::Lowest, Delimiter::RParen)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_token("`)`");
            parser.log_unmatched_delimiter(&open_paren);
            Err(ErrorsEmitted)
        }?;

        let expr = MethodCallExpr {
            receiver: Box::new(assignee_expr),
            method_name,
            open_paren,
            args_opt,
            close_paren,
        };

        Ok(Expression::MethodCall(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_method_call_expr_with_args() -> Result<(), ()> {
        let input = r#"receiver.method()"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_method_call_expr_without_args() -> Result<(), ()> {
        let input = r#"receiver.method()"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
