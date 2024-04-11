use crate::{
    ast::{
        expression::{BlockExpr, ForInStmt, GroupedExpr, IfStmt, LetStmt, TernaryStmt},
        Keyword,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{expression_collection::ParseExpressionCollection, Parser, Precedence};

pub(crate) trait ParseStatement
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

impl ParseStatement for LetStmt {
    fn parse(parser: &mut Parser) -> Result<LetStmt, ErrorsEmitted> {
        let kw_let = parser.expect_keyword(Token::Let {
            name: "let".to_string(),
            span: parser.stream.span(),
        })?;

        let assignee = parser.parse_expression(Precedence::Path)?;

        let equals = parser.expect_separator(Token::Equals {
            punc: '=',
            span: parser.stream.span(),
        })?;

        let type_ann_opt = if let Ok(colon) = parser.expect_separator(Token::Colon {
            punc: ':',
            span: parser.stream.span(),
        }) {
            let ty = parser.get_type()?;
            Some((colon, ty))
        } else {
            None
        };

        let value_opt = if let Ok(equals) = parser.expect_binary_op(Token::Equals {
            punc: '=',
            span: parser.stream.span(),
        }) {
            let value = parser.parse_expression(Precedence::Lowest)?;
            Some((equals, value))
        } else {
            None
        };

        let semicolon = parser.expect_separator(Token::Semicolon {
            punc: ';',
            span: parser.stream.span(),
        })?;

        Ok(LetStmt {
            kw_let,
            assignee,
            type_ann_opt,
            value_opt,
            semicolon,
        })
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

impl ParseStatement for TernaryStmt {
    fn parse(parser: &mut Parser) -> Result<TernaryStmt, ErrorsEmitted> {
        todo!()
    }
}

impl ParseStatement for ForInStmt {
    fn parse(parser: &mut Parser) -> Result<ForInStmt, ErrorsEmitted> {
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

        Ok(ForInStmt {
            kw_for,
            assignee,
            kw_in,
            iterable,
            block,
        })
    }
}
