use crate::{
    ast::{Expression, Type, TypeCastExpr, ValueExpr},
    error::ErrorsEmitted,
    token::TokenType,
};

use super::Parser;

impl TypeCastExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let operand = Box::new(ValueExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?);

        let kw_as = parser.expect_keyword(TokenType::As)?;

        let new_type = Box::new(Type::parse(parser)?);

        let expr = TypeCastExpr {
            operand,
            kw_as,
            new_type,
        };

        Ok(Expression::TypeCast(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_type_cast_expr() -> Result<(), ()> {
        let input = r#"x as u64"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
