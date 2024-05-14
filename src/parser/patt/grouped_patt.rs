use crate::{
    ast::{Delimiter, GroupedPatt, Pattern, TuplePatt, TuplePattElements},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::Parser,
    span::Position,
    token::Token,
};

impl GroupedPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `GroupedPatt::parse()`"),
        );
        parser.log_current_token(false);

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            let position = Position::new(parser.current, &parser.stream.span().input());
            parser.next_token();
            Ok(Delimiter::LParen { position })
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        if let Some(Token::RParen { .. }) = parser.current_token() {
            let tuple_patt = TuplePatt {
                tuple_patt_elements: TuplePattElements {
                    elements: Vec::new(),
                    final_element_opt: None,
                },
            };

            let pattern = Box::new(Pattern::TuplePatt(tuple_patt));

            return Ok(Pattern::GroupedPatt(GroupedPatt { pattern }));
        }

        let pattern = Box::new(parser.parse_pattern()?);

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                parser.next_token();

                parser.logger.log(
                    LogLevel::Debug,
                    LogMsg::from("exiting `GroupedPatt::parse()`"),
                );
                parser.log_current_token(false);

                Ok(Pattern::GroupedPatt(GroupedPatt { pattern }))
            }
            Some(Token::EOF) | None => {
                parser.log_unmatched_delimiter(&open_paren);
                parser.log_missing_token("`)`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`)`");
                Err(ErrorsEmitted)
            }
        }
    }
}
