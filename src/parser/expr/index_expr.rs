use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, IndexExpr},
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser, Precedence},
    token::Token,
};

impl ParseOperatorExpr for IndexExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let array: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_bracket = match parser.current_token() {
            Some(Token::LBracket { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LBracket { position })
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`[`");
                Err(ErrorsEmitted)
            }
        }?;

        let index = parser.parse_value_expr(Precedence::Lowest)?;

        match parser.current_token() {
            Some(Token::RBracket { .. }) => {
                parser.next_token();

                let expr = IndexExpr {
                    array: Box::new(array),
                    index: Box::new(index),
                };

                Ok(Expression::Index(expr))
            }
            _ => {
                parser.log_unmatched_delimiter(&open_bracket);
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
    fn parse_index_expr_uint() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_index_expr_identifier() -> Result<(), ()> {
        let input = r#"array[index]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
