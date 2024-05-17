use crate::{
    ast::{CallExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    parser::{collection, ParseOperatorExpr, Parser, Precedence},
    token::Token,
};

impl ParseOperatorExpr for CallExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let callee = left_expr.try_to_assignee_expr(parser)?;

        let open_paren = match parser.current_token() {
            Some(Token::LParen { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LParen { position })
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let args_opt = collection::get_expressions(parser, Precedence::Lowest, &open_paren)?;

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                parser.next_token();
                Ok(Expression::Call(CallExpr { callee, args_opt }))
            }
            _ => {
                parser.log_unmatched_delimiter(&open_paren);
                Err(ErrorsEmitted)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_call_expr() -> Result<(), ()> {
        let input = r#"foo(b"bar", -10, x)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
