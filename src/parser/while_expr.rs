use crate::{
    ast::{BlockExpr, GroupedExpr, WhileExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl WhileExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<WhileExpr, ErrorsEmitted> {
        let kw_while = parser.expect_keyword(Token::While {
            name: "while".to_string(),
            span: parser.stream.span(),
        })?;

        let condition = GroupedExpr::parse(parser)?;

        let block = BlockExpr::parse(parser)?;

        Ok(WhileExpr {
            kw_while,
            condition,
            block,
        })
    }
}
