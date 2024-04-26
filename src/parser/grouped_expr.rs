use crate::{
    ast::{Delimiter, Expression, GroupedExpr},
    error::ErrorsEmitted,
    token::TokenType,
};

use super::{Parser, Precedence};

impl GroupedExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `GroupedExpr::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let expression = parser.parse_expression(Precedence::Lowest)?;

        println!(
            "CURRENT TOKEN AFTER INNER EXPRESSION: {:?}",
            parser.peek_current()
        );

        let close_paren = parser.expect_delimiter(TokenType::RParen)?;

        let expr = GroupedExpr {
            open_paren: Delimiter::LParen,
            expression: Box::new(expression),
            close_paren,
        };

        Ok(Expression::Grouped(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_grouped_expr() -> Result<(), ()> {
        let input = r#"(x + 2)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
