use crate::{
    ast::{Identifier, IdentifierPatt, Keyword, Pattern},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    token::Token,
};

use super::Parser;

impl IdentifierPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg("entering `get_identifier_patt()`".to_string()),
        );
        parser.log_current_token(true);

        let kw_ref_opt = if let Some(Token::Ref { .. }) = parser.current_token() {
            let next_token = parser.next_token();
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

        let name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("identifier");
            Err(ErrorsEmitted)
        }?;

        parser.logger.log(
            LogLevel::Debug,
            LogMsg("exiting `get_identifier_patt()`".to_string()),
        );
        parser.log_current_token(false);

        let patt = IdentifierPatt {
            kw_ref_opt,
            kw_mut_opt,
            name,
        };

        Ok(Pattern::IdentifierPatt(patt))
    }
}