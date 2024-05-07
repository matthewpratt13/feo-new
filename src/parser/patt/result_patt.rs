use crate::{
    ast::{GroupedPatt, Keyword, Pattern, ResultPatt},
    error::ErrorsEmitted,
    parser::Parser,
    token::Token,
};

impl ResultPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        let token = parser.next_token();

        let kw_ok_or_err = match token {
            Some(Token::Ok { .. }) => Ok(Keyword::Ok),
            Some(Token::Err { .. }) => Ok(Keyword::Err),
            _ => {
                parser.log_unexpected_token("`Ok` or `Err`");
                Err(ErrorsEmitted)
            }
        }?;

        let pattern = if let Some(Token::LParen { .. }) = parser.current_token() {
            Ok(Box::new(GroupedPatt::parse(parser)?))
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let patt = ResultPatt {
            kw_ok_or_err,
            pattern,
        };

        Ok(Pattern::ResultPatt(patt))
    }
}
