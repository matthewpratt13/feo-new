use crate::{
    ast::{BlockExpr, Delimiter, InnerAttr, Statement},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl BlockExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        println!("ENTER `BlockExpr::parse()`");

        let mut attributes: Vec<InnerAttr> = Vec::new();

        while let Some(ia) = parser.get_inner_attr() {
            attributes.push(ia);
            parser.consume_token();
        }

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

            let statement = match parser.parse_statement() {
                Ok(s) => Ok(s),
                Err(_) => {
                    parser.log_unexpected_token("statement".to_string());
                    Err(ErrorsEmitted(()))
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
        })
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
