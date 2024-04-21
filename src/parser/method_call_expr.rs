use crate::{
    ast::{Expression, Identifier, MethodCallExpr, Separator},
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

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let arg_expr = parser.parse_expression(Precedence::Lowest)?;
            args.push(arg_expr);

            let curr_token = parser.peek_current();

            match curr_token {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,

                _ => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: ')' });
                }
            }
        }

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        Ok(MethodCallExpr {
            receiver: Box::new(receiver),
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
