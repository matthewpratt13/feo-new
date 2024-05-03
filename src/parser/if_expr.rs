use crate::{
    ast::{BlockExpr, Expression, GroupedExpr, IfExpr, Keyword},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{
    parse::{ParseConstruct, ParseControl},
    test_utils::log_token,
    Parser,
};

impl ParseControl for IfExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        log_token(parser, "enter `IfExpr::parse()`", true);

        let kw_if = parser.expect_keyword(TokenType::If)?;

        let mut else_if_blocks: Vec<(Keyword, Box<Expression>)> = Vec::new();
        let mut trailing_else_block_opt: Option<(Keyword, Box<Expression>)> = None;

        let condition = GroupedExpr::parse(parser)?;

        let if_block = BlockExpr::parse(parser)?;

        while let Some(Token::Else { .. }) = parser.current_token() {
            parser.next_token();

            if let Some(Token::If { .. }) = parser.current_token() {
                let if_expr = Box::new(IfExpr::parse(parser)?);
                else_if_blocks.push((Keyword::Else, if_expr));
            } else {
                continue;
            }

            if let Some(Token::LBrace { .. }) = parser.current_token() {
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
                match else_if_blocks.is_empty() {
                    true => None,
                    false => Some(else_if_blocks),
                }
            },
            trailing_else_block_opt,
        };

        log_token(parser, "exit `IfExpr::parse()`", true);

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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
