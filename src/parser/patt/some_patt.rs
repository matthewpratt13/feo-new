use crate::{
    ast::{Keyword, SomePatt},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::{Token, TokenType},
};

impl ParsePattern for SomePatt {
    fn parse_patt(parser: &mut Parser) -> Result<SomePatt, ErrorsEmitted> {
        let kw_some = if let Some(Token::Some { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Some)
        } else {
            parser.log_unexpected_token(&TokenType::Some.to_string());
            Err(ErrorsEmitted)
        }?;

        let pattern = Box::new(parser.expect_grouped_patt()?);

        Ok(SomePatt { kw_some, pattern })
    }
}
