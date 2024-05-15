use crate::{
    ast::{
        AssigneeExpr, Delimiter, Expression, Separator, TupleElements, TupleExpr, TupleIndexExpr,
    },
    error::ErrorsEmitted,
    parser::{ParseConstruct, ParseOperation, Parser, Precedence},
    span::Position,
    token::Token,
};

impl ParseConstruct for TupleExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let open_paren = match parser.current_token() {
            Some(Token::LParen { .. }) => {
                let position = Position::new(
                    parser.current_token().unwrap().span().start(),
                    &parser.stream.span().input(),
                );
                parser.next_token();
                Ok(Delimiter::LParen { position })
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let tuple_elements = parse_tuple_elements(parser)?;

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                parser.next_token();
                Ok(Expression::Tuple(TupleExpr { tuple_elements }))
            }
            _ => {
                parser.log_unmatched_delimiter(&open_paren);
                Err(ErrorsEmitted)
            }
        }
    }
}

fn parse_tuple_elements(parser: &mut Parser) -> Result<TupleElements, ErrorsEmitted> {
    let mut elements = Vec::new();
    let mut final_element_opt = None::<Box<Expression>>;

    while !matches!(
        parser.current_token(),
        Some(Token::RParen { .. } | Token::EOF)
    ) {
        let element = parser.parse_expression(Precedence::Lowest)?;

        if let Some(Token::Comma { .. }) = parser.current_token() {
            elements.push((element, Separator::Comma));
            parser.next_token();
        } else if !matches!(parser.current_token(), Some(Token::RParen { .. })) {
            parser.log_unexpected_token("`,` or `)`");
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

impl ParseOperation for TupleIndexExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let assignee_expr = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let index = match parser.current_token() {
            Some(Token::UIntLiteral { value, .. }) => {
                parser.next_token();
                Ok(value)
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("tuple index (unsigned decimal integer)");
                Err(ErrorsEmitted)
            }
        }?;

        let expr = TupleIndexExpr {
            tuple: Box::new(assignee_expr),
            index,
        };

        Ok(Expression::TupleIndex(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_tuple_expr() -> Result<(), ()> {
        let input = r#"(true, "foo", 10)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_tuple_index_expr() -> Result<(), ()> {
        let input = r#"tuple.0"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
