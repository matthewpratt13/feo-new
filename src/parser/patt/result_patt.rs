use crate::{
    ast::{GroupedPatt, Keyword, ResultPatt},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for ResultPatt {
    fn parse_patt(parser: &mut Parser) -> Result<ResultPatt, ErrorsEmitted> {
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

        Ok(ResultPatt {
            kw_ok_or_err,
            pattern,
        })
    }
}
