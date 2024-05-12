use crate::{
    ast::{Delimiter, Expression, GroupedExpr, TupleElements, TupleExpr},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{ParseConstruct, Parser, Precedence},
    token::Token,
};

impl ParseConstruct for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `GroupedExpr::parse()`"),
        );
        parser.log_current_token(false);

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        if let Some(Token::RParen { .. }) = parser.current_token() {
            let tuple_expr = TupleExpr {
                open_paren: open_paren.clone(),
                tuple_elements: TupleElements {
                    elements: Vec::new(),
                    final_element_opt: None,
                },
                close_paren: Delimiter::RParen,
            };

            let expression = Box::new(Expression::Tuple(tuple_expr));

            let grouped_expr = GroupedExpr {
                open_paren,
                expression,
                close_paren: Delimiter::RParen,
            };

            return Ok(Expression::Grouped(grouped_expr));
        }

        let expression = Box::new(parser.parse_expression(Precedence::Lowest)?);

        let close_paren = if let Some(Token::RParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_token("`)`");
            parser.log_unmatched_delimiter(&open_paren);
            Err(ErrorsEmitted)
        }?;

        let expr = GroupedExpr {
            open_paren,
            expression,
            close_paren,
        };

        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("exiting `GroupedExpr::parse()`"),
        );
        parser.log_current_token(false);

        Ok(Expression::Grouped(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_grouped_expr() -> Result<(), ()> {
        let input = r#"(x + 2)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
