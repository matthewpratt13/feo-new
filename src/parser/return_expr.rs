use crate::{
    ast::{Expression, ReturnExpr},
    error::ErrorsEmitted,
    token::TokenType,
};

use super::{Parser, Precedence};

impl ReturnExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_return = parser.expect_keyword(TokenType::Return)?;

        let expression_opt: Option<Box<Expression>> = if let Some(t) = parser.peek_current() {
            Some(Box::new(parser.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };


        let expr = ReturnExpr {
            kw_return,
            expression_opt,
        };

        Ok(Expression::Return(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_return_expr() -> Result<(), ()> {
        let input = r#"return foo;"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
