use crate::{
    ast::{BlockExpr, Delimiter, Keyword, Statement},
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

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        println!("CURRENT TOKEN (AFTER `{{`): {:?}\n", parser.peek_current());

        let mut statements: Vec<Statement> = Vec::new();

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let statement = parser.parse_statement()?;
            statements.push(statement);
        }

        println!("EXIT `parser.parse_statement()` WHILE LOOP");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());
        println!("BLOCK EXPRESSION STATEMENTS: {:#?}\n", statements.clone());

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

        Ok(BlockExpr {
            kw_unsafe_opt,
            open_brace,
            statements_opt: {
                if statements.is_empty() {
                    Some(statements)
                } else {
                    None
                }
            },
            close_brace,
        })
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
