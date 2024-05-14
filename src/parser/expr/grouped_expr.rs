use crate::{
    ast::{Delimiter, Expression, GroupedExpr, TupleElements, TupleExpr},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{ParseConstruct, Parser, Precedence},
    span::Position,
    token::Token,
};

impl ParseConstruct for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `GroupedExpr::parse()`"),
        );
        parser.log_current_token(false);

        let open_paren = match parser.current_token() {
            Some(Token::LParen { .. }) => {
                let position = Position::new(parser.current, &parser.stream.span().input());
                parser.next_token();
                Ok(Delimiter::LParen { position })
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        if let Some(Token::RParen { .. }) = parser.current_token() {
            let tuple_expr = TupleExpr {
                tuple_elements: TupleElements {
                    elements: Vec::new(),
                    final_element_opt: None,
                },
            };

            let expression = Box::new(Expression::Tuple(tuple_expr));

            return Ok(Expression::Grouped(GroupedExpr { expression }));
        }

        let expression = Box::new(parser.parse_expression(Precedence::Lowest)?);

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                parser.next_token();

                parser.logger.log(
                    LogLevel::Debug,
                    LogMsg::from("exiting `GroupedExpr::parse()`"),
                );
                parser.log_current_token(false);

                Ok(Expression::Grouped(GroupedExpr { expression }))
            }
            Some(Token::EOF) | None => {
                parser.log_unmatched_delimiter(&open_paren);
                parser.log_missing_token("`)`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`)`");
                Err(ErrorsEmitted)
            }
        }
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
