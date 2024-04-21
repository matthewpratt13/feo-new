use crate::{
    ast::{CallExpr, Delimiter, Expression},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl CallExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        callee: Expression,
    ) -> Result<CallExpr, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new();

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let arg_expr = parser.parse_expression(Precedence::Lowest)?;
            args.push(arg_expr);

            let token = parser.peek_current();

            match token {
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

        Ok(CallExpr {
            callee: Box::new(callee),
            open_paren: Delimiter::LParen,
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
    fn parse_call_expr() -> Result<(), ()> {
        let input = r#"foo(b"bar", x, -10)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
