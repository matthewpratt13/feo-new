use core::fmt;

use crate::{
    ast::{AssigneeExpr, Expression, IndexExpr},
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser, Precedence},
    span::Spanned,
    token::{Token, TokenType},
};

impl ParseOperatorExpr for IndexExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let array: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.emit_error(e);
            ErrorsEmitted
        })?;

        let open_bracket = parser.expect_open_bracket()?;

        let index = parser.parse_value_expr(Precedence::Lowest)?;

        let token = parser.current_token();

        match &token {
            Some(Token::RBracket { .. }) => {
                let span = parser.get_span(&left_expr_span, &token.unwrap().span());
                parser.next_token();

                let expr = IndexExpr {
                    array: Box::new(array),
                    index: Box::new(index),
                    span,
                };

                Ok(Expression::Index(expr))
            }
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.warn_unmatched_delimiter(&open_bracket);
                Err(ErrorsEmitted)
            }

            _ => {
                parser.emit_unexpected_token(&TokenType::RBracket.to_string());
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for IndexExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IndexExpr")
            .field("array", &self.array)
            .field("index", &self.index)
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
    fn parse_index_expr_uint() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_index_expr_identifier() -> Result<(), ()> {
        let input = r#"array[index]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
