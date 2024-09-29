use crate::{
    ast::{AssigneeExpr, Expression, TupleElements, TupleExpr, TupleIndexExpr},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, ParseOperatorExpr, Parser, Precedence},
    span::Spanned,
    token::{Token, TokenType},
};

use core::fmt;

impl ParseConstructExpr for TupleExpr {
    fn parse(parser: &mut Parser) -> Result<TupleExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        parser.expect_open_paren()?;

        let tuple_elements = parse_tuple_elements(parser)?;

        let span = parser.get_parenthesized_item_span(first_token.as_ref())?;

        Ok(TupleExpr {
            tuple_elements,
            span,
        })
    }
}

impl fmt::Debug for TupleExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TupleExpr")
            .field("tuple_elements", &self.tuple_elements)
            .finish()
    }
}

fn parse_tuple_elements(parser: &mut Parser) -> Result<TupleElements, ErrorsEmitted> {
    let mut elements: Vec<Expression> = Vec::new();
    let mut final_element_opt = None::<Box<Expression>>;

    while !matches!(
        parser.current_token(),
        Some(Token::RParen { .. } | Token::EOF)
    ) {
        let element = parser.parse_expression(Precedence::Lowest)?;

        if let Some(Token::Comma { .. }) = parser.current_token() {
            elements.push(element);
            parser.next_token();
        } else if !matches!(parser.current_token(), Some(Token::RParen { .. })) {
            parser.emit_unexpected_token(&format!("{} or {}", TokenType::Comma, TokenType::RParen));
        } else if matches!(parser.current_token(), Some(Token::RParen { .. })) {
            final_element_opt = Some(Box::new(element));
            break;
        }
    }

    Ok(TupleElements {
        elements,
        final_element_opt,
    })
}

impl ParseOperatorExpr for TupleIndexExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let tuple: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.emit_error(e);
            ErrorsEmitted
        })?;

        let index = match parser.current_token().cloned() {
            Some(Token::UIntLiteral { value, .. }) => {
                parser.next_token();
                Ok(value)
            }
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.emit_unexpected_token("tuple index (unsigned decimal integer)");
                Err(ErrorsEmitted)
            }
        }?;

        let last_token = parser.peek_behind_by(1);

        let span = parser.get_span(left_expr_span, &last_token.unwrap().span());

        let expr = TupleIndexExpr {
            tuple: Box::new(tuple),
            index,
            span,
        };

        Ok(Expression::TupleIndex(expr))
    }
}

impl fmt::Debug for TupleIndexExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TupleIndexExpr")
            .field("tuple", &self.tuple)
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
    fn parse_tuple_expr() -> Result<(), ()> {
        let input = r#"(true, "foo", 10)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_tuple_index_expr() -> Result<(), ()> {
        let input = r#"tuple.0"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
