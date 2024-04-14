use crate::{
    ast::{Expression, FieldAccessExpr, Identifier, Separator},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl FieldAccessExpr {
    pub(crate) fn parse(parser: &mut Parser, object: Expression) -> Result<FieldAccessExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        if let Some(Token::Identifier { name, .. }) = token {
            Ok(FieldAccessExpr {
                object: Box::new(object),
                dot: Separator::Dot,
                field: Identifier(name),
            })
        } else if let Some(t) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: t,
            });
            Err(ErrorsEmitted(()))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        }
    }
}

#[cfg(test)]
#[cfg(test)]
mod tests {

    use crate::test_utils;

    #[test]
    fn test_field_access_expr() -> Result<(), ()> {
        let input = r#"object.field"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}