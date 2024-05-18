use crate::{
    ast::{Delimiter, GroupedPatt, Pattern, TuplePatt, TuplePattElements},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{ParsePattern, Parser},
    span::Position,
    token::Token,
};

impl ParsePattern for GroupedPatt {
    fn parse_patt(parser: &mut Parser) -> Result<GroupedPatt, ErrorsEmitted> {
        // **log event and current token** [REMOVE IN PROD]
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

            let inner_pattern = Box::new(Pattern::TuplePatt(tuple_patt));

            return Ok(GroupedPatt { inner_pattern });
        }

        let inner_pattern = Box::new(parser.parse_pattern()?);

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                parser.next_token();

                // **log event and current token** [REMOVE IN PROD]
                parser.logger.log(
                    LogLevel::Debug,
                    LogMsg::from("exiting `GroupedPatt::parse()`"),
                );
                parser.log_current_token(false);

                Ok(GroupedPatt { inner_pattern })
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
