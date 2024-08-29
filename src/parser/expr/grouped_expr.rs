use crate::{
    ast::{Expression, GroupedExpr, TupleElements, TupleExpr},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, Parser, Precedence},
    token::Token,
};

use core::fmt;

impl ParseConstructExpr for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<GroupedExpr, ErrorsEmitted> {
        parser.logger.debug("entering `GroupedExpr::parse()`");
        parser.log_current_token(false);

        let first_token = parser.current_token().cloned();

        parser.expect_open_paren()?;

        if let Some(Token::RParen { .. }) = parser.current_token() {
            let span = parser.get_span_by_token(&first_token.unwrap());

            let tuple_expr = TupleExpr {
                tuple_elements: TupleElements {
                    elements: Vec::new(),
                    final_element_opt: None,
                },
                span: span.clone(),
            };

            let inner_expression = Expression::Tuple(tuple_expr);

            parser.next_token();

            return Ok(GroupedExpr {
                inner_expression: Box::new(inner_expression),
                span,
            });
        }

        let inner_expression = Box::new(parser.parse_expression(Precedence::Lowest)?);

        let span = parser.get_parenthesized_item_span(first_token.as_ref())?;

        parser.logger.debug("exiting `GroupedExpr::parse()`");
        parser.log_current_token(false);

        Ok(GroupedExpr {
            inner_expression,
            span,
        })
    }
}

impl fmt::Debug for GroupedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GroupedExpr")
            .field("inner_expression", &self.inner_expression)
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
    fn parse_grouped_expr() -> Result<(), ()> {
        let input = r#"(x + 2)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
