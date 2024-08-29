use crate::{
    ast::{IdentifierPatt, Keyword},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for IdentifierPatt {
    fn parse_patt(parser: &mut Parser) -> Result<IdentifierPatt, ErrorsEmitted> {
        parser.logger.trace("entering `IdentifierPatt:parse()`");
        parser.log_current_token(false);

        let kw_ref_opt = if let Some(Token::Ref { .. }) = parser.current_token() {
            parser.next_token();
            Some(Keyword::Ref)
        } else {
            None
        };

        let kw_mut_opt = if let Some(Token::Mut { .. }) = parser.current_token() {
            parser.next_token();
            Some(Keyword::Mut)
        } else {
            None
        };

        let name = parser.expect_identifier("identifier name")?;

        parser.logger.trace("exiting `IdentifierPatt::parse()`");
        parser.log_current_token(false);

        Ok(IdentifierPatt {
            kw_ref_opt,
            kw_mut_opt,
            name,
        })
    }
}
