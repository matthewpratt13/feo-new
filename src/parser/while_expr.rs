use crate::{
    ast::{BlockExpr, Expression, GroupedExpr, WhileExpr},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::Parser;

impl WhileExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_while = parser.expect_keyword(TokenType::While)?;

        let condition = if let Some(Token::LParen { .. }) = parser.peek_current() {
            Ok(Box::new(GroupedExpr::parse(parser)?))
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let block = if let Some(Token::LBrace { .. }) = parser.peek_current() {
            Ok(Box::new(BlockExpr::parse(parser)?))
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let expr = WhileExpr {
            kw_while,
            condition,
            block,
        };

        Ok(Expression::While(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_while_expr() -> Result<(), ()> {
        let input = r#"
        while (x < 5) {
            x += 1;
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
