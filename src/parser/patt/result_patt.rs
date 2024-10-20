use crate::{
    ast::{Keyword, ResultPatt},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::{Token, TokenType},
};

impl ParsePattern for ResultPatt {
    fn parse_patt(parser: &mut Parser) -> Result<ResultPatt, ErrorsEmitted> {
        let kw_ok_or_err = match parser.current_token() {
            Some(Token::Ok { .. }) => Ok(Keyword::Ok),
            Some(Token::Err { .. }) => Ok(Keyword::Err),
            _ => {
                parser.emit_unexpected_token(&format!("{} or {}", TokenType::Ok, TokenType::Err));
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let pattern = Box::new(parser.expect_grouped_patt()?);

        Ok(ResultPatt {
            kw_ok_or_err,
            pattern,
        })
    }
}
