use crate::{
    ast::{Expression, ReturnExpr},
    error::ErrorsEmitted,
    token::TokenType,
};

use super::{parse::ParseConstruct, Parser, Precedence};

impl ParseConstruct for ReturnExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_return = parser.expect_keyword(TokenType::Return)?;

        let expression_opt = match parser.current_token().is_some() {
            true => Some(Box::new(parser.parse_expression(Precedence::Lowest)?)),
            false => None,
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
