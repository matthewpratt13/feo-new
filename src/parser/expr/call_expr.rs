use crate::{
    ast::{AssigneeExpr, CallExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    parser::{collection, ParseOperatorExpr, Parser, Precedence},
    span::Spanned,
    token::Token,
};

impl ParseOperatorExpr for CallExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let callee: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

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

        let last_token = parser.current_token();

        match &last_token {
            Some(Token::RParen { .. }) => {
                let span = parser.get_span(left_expr_span, &last_token.unwrap().span());

                parser.next_token();

                Ok(Expression::Call(CallExpr {
                    callee,
                    args_opt,
                    span,
                }))
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
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_call_expr() -> Result<(), ()> {
        let input = r#"foo(b"bar", -10, x)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
