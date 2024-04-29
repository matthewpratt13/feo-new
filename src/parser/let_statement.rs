use crate::{
    ast::{Keyword, LetStmt, Type},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl LetStmt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<LetStmt, ErrorsEmitted> {
        let kw_let = if let Some(Token::Let { .. }) = parser.consume_token() {
            Ok(Keyword::Let)
        } else {
            parser.log_unexpected_str("`let`");
            Err(ErrorsEmitted)
        }?;

        let assignee = parser.get_identifier_patt()?;

        let type_ann_opt = if let Some(Token::Colon { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Type::parse(parser)?)
        } else {
            None
        };

        let value_opt = if let Some(Token::Equals { .. }) = parser.consume_token() {
            let value = parser.parse_expression(Precedence::Lowest)?;
            Some(value)
        } else {
            None
        };

        parser.consume_token();
        
        parser.expect_separator(TokenType::Semicolon)?;

        Ok(LetStmt {
            kw_let,
            assignee,
            type_ann_opt,
            value_opt,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_let_stmt() -> Result<(), ()> {
        let input = r#"let x: str = "hello world";"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
