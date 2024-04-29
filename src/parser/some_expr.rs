use crate::{
    ast::{Expression, SomeExpr},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl SomeExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_some = parser.expect_keyword(TokenType::Some)?;


        if let Some(Token::LParen { .. }) = parser.peek_current() {
            parser.consume_token();
        } else {
            parser.log_unexpected_token(TokenType::LParen);
        }

        let expression = parser.parse_expression(Precedence::Lowest)?;

        parser.consume_token();

        parser.expect_delimiter(TokenType::RBrace)?;

        let expr = SomeExpr {
            kw_some,
            expression: Box::new(expression),
        };

        Ok(Expression::SomeExpr(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_some_expr() -> Result<(), ()> {
        let input = r#"Some(foo);"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_none_expr() -> Result<(), ()> {
        let input = r#"None"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
