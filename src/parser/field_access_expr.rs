use crate::{
    ast::{FieldAccessExpr, Identifier, PlaceExpr, Separator},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl FieldAccessExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        object: PlaceExpr,
    ) -> Result<FieldAccessExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        if let Some(Token::Identifier { name, .. }) = token {
            Ok(FieldAccessExpr {
                object: Box::new(object),
                dot: Separator::Dot,
                field: Identifier(name),
            })
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        }
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
