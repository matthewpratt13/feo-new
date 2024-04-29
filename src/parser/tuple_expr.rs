use crate::{
    ast::{
        AssigneeExpr, Delimiter, Expression, Separator, TupleElements, TupleExpr, TupleIndexExpr,
    },
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl TupleExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let open_paren = if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let mut elements: Vec<(Expression, Separator)> = Vec::new();

        let mut final_element_opt = None::<Box<Expression>>;

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let element = match parser.parse_expression(Precedence::Lowest) {
                Ok(e) => Ok(e),
                Err(_) => {
                    parser.log_unexpected_str("tuple element");
                    Err(ErrorsEmitted)
                }
            }?;

            parser.consume_token();

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    elements.push((element, Separator::Comma));
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => {
                    final_element_opt = Some(Box::new(element));
                    break;
                }

                Some(_) => {
                    parser.log_unexpected_str("`,` or `)`");
                }

                None => break,
            }
        }

        let tuple_elements = TupleElements {
            elements: elements.clone(),
            final_element_opt: final_element_opt.clone(),
        };

        let close_paren = parser.expect_delimiter(TokenType::RParen)?;

        let expr = TupleExpr {
            open_paren,
            elements_opt: {
                if elements.is_empty() {
                    if final_element_opt.is_none() {
                        None
                    } else {
                        Some(tuple_elements)
                    }
                } else {
                    Some(tuple_elements)
                }
            },
            close_paren,
        };

        Ok(Expression::Tuple(expr))
    }
}

impl TupleIndexExpr {
    pub(crate) fn parse(parser: &mut Parser, lhs: Expression) -> Result<Expression, ErrorsEmitted> {
        let assignee_expr = AssigneeExpr::try_from(lhs).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let index = if let Some(Token::UIntLiteral { value, .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(value)
        } else {
            parser.log_unexpected_str("unsigned integer");
            Err(ErrorsEmitted)
        }?;

        let expr = TupleIndexExpr {
            operand: Box::new(assignee_expr),
            index,
        };

        Ok(Expression::TupleIndex(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_tuple_expr() -> Result<(), ()> {
        let input = r#"(true, "foo", 10, x)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_tuple_index_expr() -> Result<(), ()> {
        let input = r#"tuple.0"#;

        let mut parser = test_utils::get_parser(input, true);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
