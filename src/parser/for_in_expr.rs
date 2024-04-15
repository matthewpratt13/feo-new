use crate::{
    ast::{BlockExpr, ForInExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl ForInExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ForInExpr, ErrorsEmitted> {
        let kw_for = parser.expect_keyword(Token::For {
            name: "for".to_string(),
            span: parser.stream.span(),
        })?;

        let assignee = parser.parse_expression(Precedence::Path)?;

        let kw_in = parser.expect_keyword(Token::In {
            name: "in".to_string(),
            span: parser.stream.span(),
        })?;

        let iterable = parser.parse_expression(Precedence::Lowest)?;

        let block = BlockExpr::parse(parser)?;

        Ok(ForInExpr {
            kw_for,
            assignee: Box::new(assignee),
            kw_in,
            iterable: Box::new(iterable),
            block,
        })
    }
}
