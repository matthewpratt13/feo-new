use crate::{
    ast::{BlockExpr, Keyword, Statement},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl BlockExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        println!("ENTER `BlockExpr::parse()`");

        let kw_unsafe_opt = if let Some(Token::Unsafe { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Keyword::Unsafe)
        } else {
            None
        };

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        println!("CURRENT TOKEN (AFTER `{{`): {:?}\n", parser.peek_current());

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

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if statements.is_empty() {
            Ok(BlockExpr {
                kw_unsafe_opt,
                open_brace,
                statements_opt: None,
                close_brace: close_brace?,
            })
        } else {
            Ok(BlockExpr {
                kw_unsafe_opt,
                open_brace,
                statements_opt: Some(statements),
                close_brace: close_brace?,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_block_expr() -> Result<(), ()> {
        let input = r#"
        unsafe {
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
