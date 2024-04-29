use crate::{
    ast::{AssigneeExpr, Expression, FieldAccessExpr, Identifier},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl FieldAccessExpr {
    pub(crate) fn parse(parser: &mut Parser, lhs: Expression) -> Result<Expression, ErrorsEmitted> {
        let assignee_expr = AssigneeExpr::try_from(lhs).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let token = parser.peek_current();

        let expr = if let Some(Token::Identifier { name, .. }) = token {
            parser.consume_token();
            Ok(FieldAccessExpr {
                object: Box::new(assignee_expr),
                field: Identifier(name),
            })
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token,
            });
            Err(ErrorsEmitted)
        }?;

        Ok(Expression::FieldAccess(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_field_access_expr() -> Result<(), ()> {
        let input = r#"object.field"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
