use crate::{
    ast::{
        expression::{BlockExpr, GroupedExpr, IfStmt, TernaryStmt},
        Keyword,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{
    expression::ParseExpression, expression_collection::ParseExpressionCollection, Parser,
    Precedence,
};

pub(crate) trait ParseStatement
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

impl ParseStatement for TernaryStmt {
    fn parse(parser: &mut Parser) -> Result<TernaryStmt, ErrorsEmitted> {
        todo!()
    }
}

impl ParseStatement for IfStmt {
    fn parse(parser: &mut Parser) -> Result<IfStmt, ErrorsEmitted> {
        let mut else_if_blocks: Vec<(Keyword, Box<IfStmt>)> = Vec::new();

        let mut trailing_else_block_opt = None::<(Keyword, BlockExpr)>;

        let kw_if = parser.expect_keyword(Token::If {
            name: "if".to_string(),
            span: parser.stream.span(),
        })?;

        parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let expr = parser.parse_expression(Precedence::Lowest)?;

        let condition = GroupedExpr::parse(parser, expr)?;

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
            parser.consume_token()?;

            if let Some(Token::LBrace { .. }) = parser.peek_current() {
                let block = BlockExpr::parse(parser)?;
                trailing_else_block_opt = Some((Keyword::Else, block));
                break;
            }

            if let Some(Token::If { .. }) = parser.peek_current() {
                let if_stmt = IfStmt::parse(parser)?;
                else_if_blocks.push((Keyword::Else, Box::new(if_stmt)));
            } else {
                break;
            }
        }

        if else_if_blocks.is_empty() {
            Ok(IfStmt {
                kw_if,
                condition,
                if_block,
                else_if_blocks_opt: None,
                trailing_else_block_opt,
            })
        } else {
            Ok(IfStmt {
                kw_if,
                condition,
                if_block,
                else_if_blocks_opt: Some(else_if_blocks),
                trailing_else_block_opt,
            })
        }
    }
}
