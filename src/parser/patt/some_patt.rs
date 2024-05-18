use crate::{
    ast::{GroupedPatt, Keyword, SomePatt},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for SomePatt {
    fn parse_patt(parser: &mut Parser) -> Result<SomePatt, ErrorsEmitted> {
        let kw_some = if let Some(Token::Some { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Some)
        } else {
            parser.log_unexpected_token("`Some`");
            Err(ErrorsEmitted)
        }?;

        let pattern = match parser.current_token() {
            Some(Token::LParen { .. }) => Ok(Box::new(GroupedPatt::parse_patt(parser)?)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        Ok(SomePatt { kw_some, pattern })
    }
}
