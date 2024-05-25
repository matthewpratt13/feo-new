use crate::{
    ast::{AssigneeExpr, Expression, FieldAccessExpr, Identifier},
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    span::Spanned,
    token::Token,
};

impl ParseOperatorExpr for FieldAccessExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let object: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let token = parser.current_token();

        let expr = match &token {
            Some(Token::Identifier { name, .. }) => {
                let field_name = Identifier::from(name);

                let span = parser.get_span(left_expr_span, &token.unwrap().span());

                parser.next_token();

                Ok(FieldAccessExpr {
                    object: Box::new(object),
                    field_name,
                    span,
                })
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("identifier in field access expression");
                Err(ErrorsEmitted)
            }
        }?;

        Ok(Expression::FieldAccess(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_field_access_expr() -> Result<(), ()> {
        let input = r#"object.field"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
