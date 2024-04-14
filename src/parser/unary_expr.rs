use crate::{
    ast::{UnaryExpr, UnaryOp},
    error::{ErrorsEmitted, ParserErrorKind},
};

use super::{Parser, Precedence};
impl UnaryExpr {
    pub(crate) fn parse(parser: &mut Parser, op: UnaryOp) -> Result<UnaryExpr, ErrorsEmitted> {
        let expression = parser.parse_expression(Precedence::Unary)?;

        parser.consume_token().ok_or({
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        })?;

        match op {
            _ => Ok(UnaryExpr {
                expression: Box::new(expression),
                op,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils;

    #[test]
    #[ignore]
    fn test_unary_expr_negate() -> Result<(), ()> {
        let input = r#"-x"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
