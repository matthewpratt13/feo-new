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
                parser.consume_token();
                break;
            }

            let arg_expr = parser.parse_expression(Precedence::Lowest)?;
            args.push(arg_expr);

            let token = parser.consume_token();

            match token {
                Some(Token::Comma { .. }) => continue,
                Some(Token::RParen { .. }) => break,
                Some(t) => {
                    parser.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: t,
                    });
                }
                None => parser.log_error(ParserErrorKind::UnexpectedEndOfInput),
            }
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if args.is_empty() {
            Ok(CallExpr {
                callee: Box::new(callee),
                open_paren: Delimiter::LParen,
                args_opt: None,
                close_paren: Delimiter::RParen,
            })
        } else {
            Ok(CallExpr {
                callee: Box::new(callee),
                open_paren: Delimiter::LParen,
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
    fn parse_call_expr() -> Result<(), ()> {
        let input = r#"foo(bar)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
