use crate::{
    ast::{BlockExpr, Delimiter, Expression, InnerAttr, Statement},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::Parser;

impl BlockExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let mut attributes: Vec<InnerAttr> = Vec::new();

        println!("enter `BlockExpr::parser()`");
        println!("current token: `{:?}`", parser.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            parser.get_precedence(&parser.peek_current().unwrap_or(Token::EOF))
        );

        while let Some(ia) = parser.get_inner_attr() {
            attributes.push(ia);
            parser.consume_token();
        }

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        println!("enter block");
        println!("current token: `{:?}`", parser.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            parser.get_precedence(&parser.peek_current().unwrap_or(Token::EOF))
        );

        let mut statements: Vec<Statement> = Vec::new();

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let statement = match parser.parse_statement() {
                Ok(s) => Ok(s),
                Err(_) => {
                    parser.log_unexpected_str("statement");
                    Err(ErrorsEmitted)
                }
            }?;

            statements.push(statement);

            match parser.peek_current() {
                Some(Token::RBracket { .. }) | None => break,
                Some(_) => {
                    continue;
                }
            }
        }

        println!("statements: {:?}", statements);

        println!("exit block");
        println!("current token: `{:?}`", parser.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            parser.get_precedence(&parser.peek_current().unwrap_or(Token::EOF))
        );

        let close_brace = parser.expect_delimiter(TokenType::RBrace)?;

        let expr = BlockExpr {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            open_brace,
            statements_opt: {
                if statements.is_empty() {
                    None
                } else {
                    Some(statements)
                }
            },
            close_brace,
        };

        println!("exit `BlockExpr::parse()`");
        println!("current token: `{:?}`", parser.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            parser.get_precedence(&parser.peek_current().unwrap_or(Token::EOF))
        );

        Ok(Expression::Block(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_block_expr() -> Result<(), ()> {
        let input = r#"
        #![unsafe]
        {
            x + 2;
            y
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
