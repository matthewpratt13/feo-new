use crate::{
    ast::{BlockExpr, Delimiter, Expression, InnerAttr, Statement},
    error::{ErrorsEmitted, ParserErrorKind},
    parser::test_utils::log_token,
    token::{Token, TokenType},
};

use super::{collection, Parser};

impl BlockExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        log_token(parser, "enter `BlockExpr::parse()`", true);

        let attributes_opt = collection::get_attributes(parser, InnerAttr::inner_attr);

        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            parser.next_token();
            log_token(parser, "consume token", true);
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let mut statements: Vec<Statement> = Vec::new();

        while !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            let statement = parser.parse_statement()?;
            statements.push(statement);

            if let Some(Token::Semicolon { .. }) = parser.current_token() {
                parser.next_token();
            }
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = BlockExpr {
            attributes_opt,
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

        log_token(parser, "exit `BlockExpr::parser()`", true);
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
