use crate::{
    ast::{GroupedPatt, Keyword, Pattern, ResultPatt},
    error::ErrorsEmitted,
    parser::Parser,
    token::Token,
};

impl ResultPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        let kw_ok_or_err = match parser.current_token() {
            Some(Token::Ok { .. }) => Ok(Keyword::Ok),
            Some(Token::Err { .. }) => Ok(Keyword::Err),
            _ => {
                parser.log_unexpected_token("`Ok` or `Err`");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let pattern = match parser.current_token() {
            Some(Token::LParen { .. }) => Ok(Box::new(GroupedPatt::parse(parser)?)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let patt = ResultPatt {
            kw_ok_or_err,
            pattern,
        };

        Ok(Pattern::ResultPatt(patt))
    }
}
