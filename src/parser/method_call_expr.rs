use crate::{
    ast::{AssigneeExpr, Expression, Identifier, MethodCallExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl MethodCallExpr {
    pub(crate) fn parse(parser: &mut Parser, lhs: Expression) -> Result<Expression, ErrorsEmitted> {
        let receiver = AssigneeExpr::try_from(lhs).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let token = parser.peek_current();

        let method_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token,
            });
            Err(ErrorsEmitted)
        }?;
        
        parser.consume_token();
        
        let open_paren = parser.expect_delimiter(TokenType::LParen)?;

        let args = MethodCallExpr::parse_arguments(parser)?;

        let close_paren = parser.expect_delimiter(TokenType::RParen)?;

        let expr = MethodCallExpr {
            receiver: Box::new(receiver),
            method_name,
            open_paren,
            args_opt: {
                if args.is_empty() {
                    None
                } else {
                    Some(args)
                }
            },
            close_paren,
        };

        Ok(Expression::MethodCall(expr))
    }

    fn parse_arguments(parser: &mut Parser) -> Result<Vec<Expression>, ErrorsEmitted> {
        let mut arguments = Vec::new();

        // Parse comma-separated arguments
        while !matches!(
            parser.peek_current(),
            Some(Token::RParen { .. } | Token::EOF)
        ) {
            let argument = parser.parse_expression(Precedence::Lowest)?;
            arguments.push(argument);

            // Optionally consume comma
            if let Some(Token::Comma { .. }) = parser.peek_current() {
                parser.consume_token(); // Consume the comma
            }
        }

        Ok(arguments)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_method_call_expr_with_args() -> Result<(), ()> {
        let input = r#"receiver.method(x, "foo", -10)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_method_call_expr_without_args() -> Result<(), ()> {
        let input = r#"receiver.method()"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
