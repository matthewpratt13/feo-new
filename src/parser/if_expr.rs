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

        let kw_if = parser.expect_keyword(Token::If {
            name: "if".to_string(),
            span: parser.stream.span(),
        })?;

        let condition = GroupedExpr::parse(parser)?;

        let token = parser.peek_current().ok_or({
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        })?;

        let if_block = if let Token::LBrace { .. } = token {
            BlockExpr::parse(parser)?
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "block expression".to_string(),
                found: token,
            });
            return Err(ErrorsEmitted(()));
        };

        while let Some(Token::Else { .. }) = parser.peek_current() {
            parser.consume_token();

            if let Some(Token::LBrace { .. }) = parser.peek_current() {
                let block = BlockExpr::parse(parser)?;
                trailing_else_block_opt = Some((Keyword::Else, block));
                break;
            }

            if let Some(Token::If { .. }) = parser.peek_current() {
                let if_stmt = IfExpr::parse(parser)?;
                else_if_blocks.push((Keyword::Else, Box::new(if_stmt)));
            } else {
                break;
            }
        }

        if else_if_blocks.is_empty() {
            Ok(IfExpr {
                kw_if,
                condition,
                if_block,
                else_if_blocks_opt: None,
                trailing_else_block_opt,
            })
        } else {
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
