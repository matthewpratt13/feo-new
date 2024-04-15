use crate::{
    ast::{BlockExpr, Statement},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl BlockExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut statements: Vec<Statement> = Vec::new();

        while let Ok(s) = parser.parse_statement() {
            statements.push(s);

            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }
        }

        println!("EXIT `parser.parse_statement()` WHILE LOOP");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());
        println!("BLOCK EXPRESSION STATEMENTS: {:#?}\n", statements.clone());

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        });

        if statements.is_empty() {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "statement".to_string(),
            });
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(BlockExpr {
            open_brace,
            statements,
            close_brace: close_brace?,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_block_expr() -> Result<(), ()> {
        let input = r#"
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
