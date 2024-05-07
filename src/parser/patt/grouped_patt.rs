use crate::{
    ast::{Delimiter, GroupedPatt, Pattern},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::Parser,
    token::Token,
};

impl GroupedPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg("entering `GroupedPatt::parse()`".to_string()),
        );
        parser.log_current_token(false);

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let pattern = parser.parse_pattern()?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_token("`)`");
            parser.log_unmatched_delimiter(open_paren.clone());
            Err(ErrorsEmitted)
        }?;

        let patt = GroupedPatt {
            open_paren,
            pattern: Box::new(pattern),
            close_paren,
        };

        parser.logger.log(
            LogLevel::Debug,
            LogMsg("exiting `GroupedExpr::parse()`".to_string()),
        );
        parser.log_current_token(false);

        Ok(Pattern::GroupedPatt(patt))
    }
}
