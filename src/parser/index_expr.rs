use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, IndexExpr},
    error::ErrorsEmitted,
    token::TokenType,
};

use super::{Parser, Precedence};

impl IndexExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        array: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let array = AssigneeExpr::try_from(array).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let index = parser.parse_expression(Precedence::Lowest)?;

        let close_bracket = parser.expect_delimiter(TokenType::RBracket)?;

        let expr = IndexExpr {
            array: Box::new(array),
            open_bracket: Delimiter::LBracket,
            index: Box::new(index),
            close_bracket,
        };

        Ok(Expression::Index(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_index_expr_uint() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_index_expr_identifier() -> Result<(), ()> {
        let input = r#"array[index]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
