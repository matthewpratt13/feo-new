use crate::{
    ast::{AssigneeExpr, CallExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl CallExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        callee: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new();

        let callee = AssigneeExpr::try_from(callee).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        parser.consume_token();

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let arg_expr = match parser.parse_expression(Precedence::Lowest) {
                Ok(e) => Ok(e),
                Err(_) => {
                    parser.log_unexpected_str("function argument");
                    Err(ErrorsEmitted)
                }
            }?;

            args.push(arg_expr);

            parser.consume_token();

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,
                Some(_) => {
                    parser.log_unexpected_str("`,` or `)`");
                }
                None => break,
            }
        }

        let close_paren = parser.expect_delimiter(TokenType::RParen)?;

        let expr = CallExpr {
            callee,
            open_paren: Delimiter::LParen,
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

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
