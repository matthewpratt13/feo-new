use crate::{
    ast::{AssigneeExpr, CallExpr, Delimiter, Expression},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl CallExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        callee: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let mut args = Vec::new();

        let callee = AssigneeExpr::try_from(callee).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        while !matches!(
            parser.peek_current(),
            Some(Token::RParen { .. } | Token::EOF)
        ) {
            let arg = parser.parse_expression(Precedence::Lowest)?;
            args.push(arg);

            if let Some(Token::Comma { .. }) = parser.peek_current() {
                parser.consume_token();
            }
        }

        let close_paren = if let Some(Token::RParen { .. }) = parser.consume_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = CallExpr {
            callee,
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

        Ok(Expression::Call(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_call_expr() -> Result<(), ()> {
        let input = r#"foo(b"bar", -10, x)"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
