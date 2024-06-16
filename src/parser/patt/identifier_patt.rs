use crate::{
    ast::{Identifier, IdentifierPatt, Keyword},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for IdentifierPatt {
    fn parse_patt(parser: &mut Parser) -> Result<IdentifierPatt, ErrorsEmitted> {
        parser.logger.debug("entering `IdentifierPatt:parse()`");
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

        let name = if let Some(Token::Identifier { name, .. }) = parser.current_token().cloned() {
            parser.next_token();
            Ok(Identifier::from(&name))
        } else {
            parser.log_unexpected_token("identifier");
            Err(ErrorsEmitted)
        }?;

        let patt = IdentifierPatt {
            kw_ref_opt,
            kw_mut_opt,
            name,
        };

        parser.logger.debug("exiting `IdentifierPatt::parse()`");
        parser.log_current_token(false);

        Ok(patt)
    }
}
