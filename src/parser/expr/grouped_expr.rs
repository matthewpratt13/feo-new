use crate::{
    ast::{Delimiter, Expression, GroupedExpr, TupleElements, TupleExpr},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{ParseConstructExpr, Parser, Precedence},
    span::Spanned,
    token::Token,
};

impl ParseConstructExpr for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<GroupedExpr, ErrorsEmitted> {
        ////////////////////////////////////////////////////////////////////////////////
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `GroupedExpr::parse()`"),
        );
        parser.log_current_token(false);
        ////////////////////////////////////////////////////////////////////////////////

        let first_token = parser.current_token().cloned();

        let open_paren = match &first_token {
            Some(Token::LParen { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LParen { position })
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        if let Some(Token::RParen { .. }) = parser.current_token() {
            let token = first_token.unwrap();

            let span = parser.get_span_by_token(&token);

            let tuple_expr = TupleExpr {
                tuple_elements: TupleElements {
                    elements: Vec::new(),
                    final_element_opt: None,
                },
                span,
            };

            let inner_expression = Expression::Tuple(tuple_expr);

            let span = parser.get_span(&token.span(), &inner_expression.span());

            parser.next_token();

            return Ok(GroupedExpr {
                inner_expression: Box::new(inner_expression),
                span,
            });
        }

        let inner_expression = Box::new(parser.parse_expression(Precedence::Lowest)?);

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());

                parser.next_token();

                ////////////////////////////////////////////////////////////////////////////////
                parser.logger.log(
                    LogLevel::Debug,
                    LogMsg::from("exiting `GroupedExpr::parse()`"),
                );
                parser.log_current_token(false);
                ////////////////////////////////////////////////////////////////////////////////

                Ok(GroupedExpr {
                    inner_expression,
                    span,
                })
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
    fn parse_grouped_expr() -> Result<(), ()> {
        let input = r#"(x + 2)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
