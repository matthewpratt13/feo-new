use crate::{
    ast::{
        expression::{BlockExpr, IfStmt, TernaryStmt},
        Delimiter, Keyword, Statement,
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

impl ParseStatement for TernaryStmt {
    fn parse(parser: &mut Parser) -> Result<TernaryStmt, ErrorsEmitted> {
        todo!()
    }
}

impl ParseStatement for IfStmt {
    fn parse(parser: &mut Parser) -> Result<IfStmt, ErrorsEmitted> {
        let kw_if = parser.expect_keyword(Token::If {
            name: "if".to_string(),
            span: parser.stream.span(),
        })?;

        let condition = Box::new(parser.parse_expression(Precedence::Lowest)?);

        let token = parser.consume_token()?;

        let if_block = if let Delimiter::LBrace = parser.expect_delimiter(token)? {
            BlockExpr::parse(parser)?
        } else if let Token::QuestionMark { .. } = token {
            return Err(ErrorsEmitted(()));
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "block expression".to_string(),
                found: token,
            });

            return Err(ErrorsEmitted(()));
        };

        let mut else_if_blocks: Vec<(Keyword, Box<IfStmt>)> = Vec::new();

        while let Some(t) = parser.peek_current() {
            let else_kw = parser.expect_keyword(t)?;

            if let Some(Token::If { .. }) = parser.peek_current() {
                let if_stmt = IfStmt::parse(parser)?;
                else_if_blocks.push((else_kw, Box::new(if_stmt)))
            } else {
                break;
            }
        }

        Ok(Statement::If(condition, true_branch, false_branch))
    }
}
