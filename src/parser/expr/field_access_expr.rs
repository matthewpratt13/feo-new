use crate::{
    ast::{Expression, FieldAccessExpr, Identifier},
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    token::Token,
};

impl ParseOperatorExpr for FieldAccessExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let object = left_expr.try_to_assignee_expr(parser)?;

        let expr = match parser.current_token() {
            Some(Token::Identifier { name, .. }) => {
                parser.next_token();

                Ok(FieldAccessExpr {
                    object: Box::new(object),
                    field_name: Identifier(name),
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
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_field_access_expr() -> Result<(), ()> {
        let input = r#"object.field"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
