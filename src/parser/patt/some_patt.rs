use crate::{
    ast::{GroupedPatt, Keyword, Pattern, SomePatt},
    error::ErrorsEmitted,
    parser::Parser,
    token::Token,
};

impl SomePatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        let kw_some = if let Some(Token::Some { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Some)
        } else {
            parser.log_unexpected_token("`Some`");
            Err(ErrorsEmitted)
        }?;

        let pattern = if let Some(Token::LParen { .. }) = parser.current_token() {
            Ok(Box::new(GroupedPatt::parse(parser)?))
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let patt = SomePatt { kw_some, pattern };

        Ok(Pattern::SomePatt(patt))
    }
}
