use crate::{
    ast::{CallExpr, Delimiter, Expression},
    error::ErrorsEmitted,
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

            let arg_expr = match parser.parse_expression(Precedence::Lowest) {
                Ok(e) => Ok(e),
                Err(_) => {
                    parser.log_unexpected_token("function argument".to_string());
                    Err(ErrorsEmitted(()))
                }
            }?;

            args.push(arg_expr);

            match parser.peek_current() {
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
        let input = r#"foo(b"bar", -10, x)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
