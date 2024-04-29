use crate::{
    ast::{BlockExpr, Expression, GroupedExpr, IfExpr, Keyword},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::Parser;

impl IfExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_if = parser.expect_keyword(TokenType::If)?;

        let mut else_if_blocks: Vec<(Keyword, Box<Expression>)> = Vec::new();
        let mut trailing_else_block_opt: Option<(Keyword, Box<Expression>)> = None;

        let condition = GroupedExpr::parse(parser)?;

        let if_block = BlockExpr::parse(parser)?;

        while let Some(Token::Else { .. }) = parser.peek_current() {
            parser.consume_token();

            if let Some(Token::If { .. }) = parser.peek_current() {
                let if_expr = Box::new(IfExpr::parse(parser)?);
                else_if_blocks.push((Keyword::Else, if_expr));
            } else {
                continue;
            }

            if let Some(Token::LBrace { .. }) = parser.peek_current() {
                let block = Box::new(BlockExpr::parse(parser)?);
                trailing_else_block_opt = Some((Keyword::Else, block));
                break;
            }
        }

        let expr = IfExpr {
            kw_if,
            condition: Box::new(condition),
            if_block: Box::new(if_block),
            else_if_blocks_opt: {
                if else_if_blocks.is_empty() {
                    None
                } else {
                    Some(else_if_blocks)
                }
            },
            trailing_else_block_opt,
        };

        Ok(Expression::If(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_if_expr() -> Result<(), ()> {
        let input = r#"
        if (x < 5) {
            if (x > 2) {
                return true;
            }
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
