use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, IndexExpr, ValueExpr},
    error::ErrorsEmitted,
    parser::{ParseOperation, Parser, Precedence},
    span::Position,
    token::Token,
};

impl ParseOperation for IndexExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let assignee_expr = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_bracket = if let Some(Token::LBracket { .. }) = parser.current_token() {
            let position = Position::new(parser.current, &parser.stream.span().input());
            parser.next_token();
            Ok(Delimiter::LBracket { position })
        } else {
            parser.log_unexpected_token("`[`");
            Err(ErrorsEmitted)
        }?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let value_expr = ValueExpr::try_from(expression).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        match parser.current_token() {
            Some(Token::RBracket { .. }) => {
                parser.next_token();

                let expr = IndexExpr {
                    array: Box::new(assignee_expr),
                    index: Box::new(value_expr),
                };

                Ok(Expression::Index(expr))
            }
            Some(Token::EOF) | None => {
                parser.log_unmatched_delimiter(&open_bracket);
                parser.log_missing_token("`]`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`]`");
                Err(ErrorsEmitted)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_index_expr_uint() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_index_expr_identifier() -> Result<(), ()> {
        let input = r#"array[index]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
