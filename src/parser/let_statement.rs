use crate::{
    ast::{Keyword, LetStmt, Type},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl LetStmt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<LetStmt, ErrorsEmitted> {
        let kw_let = if let Some(Token::Let { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Let)
        } else {
            parser.log_unexpected_str("`let`");
            Err(ErrorsEmitted)
        }?;

        let assignee = parser.get_identifier_patt()?;

        let type_ann_opt = if let Some(Token::Colon { .. }) = parser.current_token() {
            parser.next_token();
            Some(Type::parse(parser)?)
        } else {
            None
        };

        let value_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();
            let value = parser.parse_expression(Precedence::Lowest)?;
            Some(value)
        } else {
            None
        };

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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
