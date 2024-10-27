use core::fmt;

use crate::{
    ast::{AssigneeExpr, CallExpr, Expression},
    error::ErrorsEmitted,
    parser::{get_expressions, ParseOperatorExpr, Parser, Precedence},
    span::Spanned,
    token::{Token, TokenType},
};

impl ParseOperatorExpr for CallExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let callee: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.emit_error(e);
            ErrorsEmitted
        })?;

        let open_paren = parser.expect_open_paren()?;

        let args_opt = get_expressions(parser, Precedence::Lowest, &open_paren)?;

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
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.warn_unmatched_delimiter(&open_paren);
                Err(ErrorsEmitted)
            }
            _ => {
                parser.emit_unexpected_token(&TokenType::RParen.to_string());
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CallExpr")
            .field("callee", &self.callee)
            .field("args_opt", &self.args_opt)
            .finish()
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
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
