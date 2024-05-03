use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, Identifier, MethodCallExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, Parser, Precedence};

impl MethodCallExpr {
    pub(crate) fn parse(parser: &mut Parser, lhs: Expression) -> Result<Expression, ErrorsEmitted> {
        let receiver = AssigneeExpr::try_from(lhs).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let token = parser.next_token();

        let method_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token,
            });
            Err(ErrorsEmitted)
        }?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.next_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let args = collection::get_expressions(parser, Precedence::Lowest)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

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
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_method_call_expr_with_args() -> Result<(), ()> {
        let input = r#"receiver.method(x, "foo", -10)"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_method_call_expr_without_args() -> Result<(), ()> {
        let input = r#"receiver.method()"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
