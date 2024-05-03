use crate::{
    ast::{
        AssigneeExpr, Delimiter, Expression, Separator, TupleElements, TupleExpr, TupleIndexExpr,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl TupleExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let mut elements = Vec::new();

        let mut final_element_opt = None::<Box<Expression>>;

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        while !matches!(
            parser.current_token(),
            Some(Token::RParen { .. } | Token::EOF)
        ) {
            let element = parser.parse_expression(Precedence::Lowest)?;

            if let Some(Token::Comma { .. }) = parser.current_token() {
                elements.push((element, Separator::Comma));
                parser.next_token();
            } else if !matches!(parser.current_token(), Some(Token::RParen { .. })) {
                parser.log_unexpected_str("`,` or `)`");
            } else if matches!(parser.current_token(), Some(Token::RParen { .. })) {
                final_element_opt = Some(Box::new(element));
                break;
            }
        }

        let tuple_elements = TupleElements {
            elements: elements.clone(),
            final_element_opt: final_element_opt.clone(),
        };

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = TupleExpr {
            open_paren,
            elements_opt: {
                match elements.is_empty() {
                    true if final_element_opt.is_none() => None,
                    true => Some(tuple_elements),
                    false => Some(tuple_elements),
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

        let index = if let Some(Token::UIntLiteral { value, .. }) = parser.current_token() {
            parser.next_token();
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_tuple_index_expr() -> Result<(), ()> {
        let input = r#"tuple.0"#;

        let mut parser = test_utils::get_parser(input, true);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
