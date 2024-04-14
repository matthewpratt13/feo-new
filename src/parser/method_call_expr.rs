use crate::{
    ast::{Delimiter, Expression, Identifier, MethodCallExpr, Separator},
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
        } else if let Some(t) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: t,
            });
            Err(ErrorsEmitted(()))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        };

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        });

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                parser.consume_token();
                break;
            }

            let arg_expr = parser.parse_expression(Precedence::Lowest)?;
            args.push(arg_expr);

            let curr_token = parser.consume_token();

            match curr_token {
                Some(Token::Comma { .. }) => continue,
                Some(Token::RParen { .. }) => break,
                _ => {
                    parser.log_error(ParserErrorKind::TokenNotFound {
                        expected: "`)`".to_string(),
                    });
                    break;
                }
            }
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if args.is_empty() {
            Ok(MethodCallExpr {
                receiver: Box::new(receiver),
                dot: Separator::Dot,
                method_name: method_name?,
                open_paren: open_paren?,
                args_opt: None,
                close_paren: Delimiter::RParen,
            })
        } else {
            Ok(MethodCallExpr {
                receiver: Box::new(receiver),
                dot: Separator::Dot,
                method_name: method_name?,
                open_paren: open_paren?,
                args_opt: Some(args),
                close_paren: Delimiter::RParen,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_method_call_expr() -> Result<(), ()> {
        let input = r#"receiver.method(foo, bar)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
