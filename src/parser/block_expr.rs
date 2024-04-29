use crate::{
    ast::{BlockExpr, Delimiter, Expression, InnerAttr, Statement},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::Parser;

impl BlockExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let mut attributes: Vec<InnerAttr> = Vec::new();

        while let Some(ia) = parser.get_inner_attr() {
            attributes.push(ia);
            parser.consume_token();
        }

        let open_brace = if let Some(Token::LBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let mut statements: Vec<Statement> = Vec::new();

        while !matches!(
            parser.peek_current(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            let statement = parser.parse_statement()?;
            statements.push(statement);

            if let Some(Token::Semicolon { .. }) = parser.peek_current() {
                parser.consume_token(); // Consume the semicolon to separate expressions/statements
            }
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

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
