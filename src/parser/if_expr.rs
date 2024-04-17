use crate::{
    ast::{BlockExpr, GroupedExpr, IfExpr, Keyword},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl IfExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<IfExpr, ErrorsEmitted> {
        let mut else_if_blocks: Vec<(Keyword, Box<IfExpr>)> = Vec::new();

        let mut trailing_else_block_opt = None::<(Keyword, BlockExpr)>;

        println!("ENTER `IfExpr::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let kw_if = parser.expect_keyword(Token::If {
            name: "if".to_string(),
            span: parser.stream.span(),
        })?;

        println!("CURRENT TOKEN AFTER `if`: {:?}\n", parser.peek_current());

        let _ = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        });

        let condition = Box::new(GroupedExpr::parse(parser)?);

        let token = parser.peek_current();

        println!(
            "CURRENT TOKEN AFTER CONDITION: {:?}\n",
            parser.peek_current()
        );

        let if_block = if let Some(Token::LBrace { .. }) = token {
            println!(
                "CURRENT TOKEN ENTERING AN IF BLOCK: {:?}\n",
                parser.peek_current()
            );

            Box::new(BlockExpr::parse(parser)?)
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "block expression".to_string(),
                found: token,
            });
            return Err(ErrorsEmitted(()));
        };

        while let Some(Token::Else { .. }) = parser.peek_current() {
            println!("CURRENT TOKEN AFTER `else`: {:?}\n", parser.peek_current());

            parser.consume_token();

            if let Some(Token::If { .. }) = parser.peek_current() {
                let if_expr = IfExpr::parse(parser)?;
                else_if_blocks.push((Keyword::Else, Box::new(if_expr)));
            } else {
                continue;
            }

            if let Some(Token::LBrace { .. }) = parser.peek_current() {
                let block = BlockExpr::parse(parser)?;
                trailing_else_block_opt = Some((Keyword::Else, block));
                break;
            }
        }

        println!("ELSE-IF BLOCKS: {:#?}\n", else_if_blocks.clone());

        if else_if_blocks.is_empty() {
            println!("EXIT `IfExpr::parse()` WITHOUT ELSE-IF BLOCKS");
            println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

            Ok(IfExpr {
                kw_if,
                condition,
                if_block,
                else_if_blocks_opt: None,
                trailing_else_block_opt,
            })
        } else {
            println!("EXIT `IfExpr::parse()` WITH ELSE-IF BLOCKS");
            println!("CURRENT TOKEN: {:?}\n", parser.peek_current());
            Ok(IfExpr {
                kw_if,
                condition,
                if_block,
                else_if_blocks_opt: Some(else_if_blocks),
                trailing_else_block_opt,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_if_expr() -> Result<(), ()> {
        let input = r#"
        if (x < 5) {
            return true;
        } else if (x == 5) {
            return true;
        } else {
            return false;
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}