use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, Identifier, MethodCallExpr, Separator},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl MethodCallExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        receiver: Expression,
    ) -> Result<MethodCallExpr, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new();

        let token = parser.consume_token();

        let method_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        }?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let arg_expr = match parser.parse_expression(Precedence::Lowest) {
                Ok(e) => Ok(e),
                Err(_) => {
                    parser.log_unexpected_token("method argument".to_string());
                    Err(ErrorsEmitted(()))
                }
            }?;

            args.push(arg_expr);

            let curr_token = parser.peek_current();

            match curr_token {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,
                Some(_) => {
                    parser.log_unexpected_token("`,` or `)`".to_string());
                }
                None => break,
            }
        }

        let close_paren = if let Some(Token::RParen { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_delimiter(')');
            Err(ErrorsEmitted(()))
        }?;

        Ok(MethodCallExpr {
            receiver: AssigneeExpr(Box::new(receiver)),
            dot: Separator::Dot,
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
        })
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
