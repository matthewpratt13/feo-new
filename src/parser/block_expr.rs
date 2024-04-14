use crate::{
    ast::{BlockExpr, Delimiter, Statement},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl BlockExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        let mut statements: Vec<Statement> = Vec::new();

        while !parser.is_expected_token(&Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        }) {
            let statement = parser.parse_statement()?;
            statements.push(statement);
        }

        let terminal_expression_opt = if let Ok(e) = parser.parse_expression(Precedence::Lowest) {
            Some(Box::new(e))
        } else {
            None
        };

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        });

        if statements.is_empty() {
            parser.log_error(ParserErrorKind::TokenNotFound {
                expected: "statement".to_string(),
            });
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(BlockExpr {
            open_brace: Delimiter::LBrace,
            statements,
            terminal_expression_opt,
            close_brace: close_brace?,
        })
    }
}
