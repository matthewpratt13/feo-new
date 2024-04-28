use crate::{
    ast::{Delimiter, Expression, GroupedExpr},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl GroupedExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        println!("enter `GroupedExpr::parse()`");
        println!("current token: `{:?}`", parser.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            parser.get_precedence(&parser.peek_current().unwrap_or(Token::EOF))
        );

        let open_paren = if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        println!("exit `parse_expression()`");
        println!("current token: `{:?}`", parser.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            parser.get_precedence(&parser.peek_current().unwrap_or(Token::EOF))
        );

        let close_paren = parser.expect_delimiter(TokenType::RParen)?;

        let expr = GroupedExpr {
            open_paren: Delimiter::LParen,
            expression: Box::new(expression),
            close_paren,
        };

        println!("exit `GroupedExpr::parse()`");
        println!("current token: `{:?}`", parser.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            parser.get_precedence(&parser.peek_current().unwrap_or(Token::EOF))
        );

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
