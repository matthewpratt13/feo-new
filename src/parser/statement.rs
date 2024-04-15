use crate::{
    ast::{BlockExpr, ExpressionStmt, GroupedExpr, LetStmt, Separator, WhileStmt},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

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

impl ParseStatement for WhileStmt {
    fn parse(parser: &mut Parser) -> Result<WhileStmt, ErrorsEmitted> {
        let kw_while = parser.expect_keyword(Token::While {
            name: "while".to_string(),
            span: parser.stream.span(),
        })?;

        let condition = GroupedExpr::parse(parser)?;

        let block = BlockExpr::parse(parser)?;

        Ok(WhileStmt {
            kw_while,
            condition,
            block,
        })
    }
}

impl ParseStatement for ExpressionStmt {
    fn parse(parser: &mut Parser) -> Result<ExpressionStmt, ErrorsEmitted> {
        println!("ENTER `ExpressionStmt::parse()`\n");

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let semicolon_opt = if let Some(Token::Semicolon { .. }) = parser.peek_current() {
            println!("ENCOUNTER `;`");

            parser.consume_token();

            println!("SKIP `;`");
            println!("CURRENT TOKEN: {:?}", parser.peek_current());

            Some(Separator::Semicolon)
        } else {
            None
        };

        println!("EXIT `ExpressionStmt::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        Ok(ExpressionStmt {
            expression,
            semicolon_opt,
        })
    }
}
