use crate::{
    ast::{AssigneeExpr, Expression, FieldAccessExpr, Identifier},
    error::{ErrorsEmitted, ParserErrorKind},
    logger::{LogLevel, LogMsg},
    parser::{ParseOperation, Parser},
    token::Token,
};

impl ParseOperation for FieldAccessExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg("entering `FieldAccessExpr::parse()`".to_string()),
        );
        parser.log_current_token(true);

        let assignee_expr = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let token = parser.next_token();

        let expr = if let Some(Token::Identifier { name, .. }) = token {
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
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_field_access_expr() -> Result<(), ()> {
        let input = r#"object.field"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
