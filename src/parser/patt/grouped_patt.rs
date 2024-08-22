use crate::{
    ast::{Delimiter, GroupedPatt, Pattern, TuplePatt, TuplePattElements},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for GroupedPatt {
    fn parse_patt(parser: &mut Parser) -> Result<GroupedPatt, ErrorsEmitted> {
        parser.logger.debug("entering `GroupedPatt::parse()`");
        parser.log_current_token(false);

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            let position = parser.current_position();
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

        let _ = parser.get_parenthesized_item_span(None, &open_paren)?;

        parser.logger.debug("entering `GroupedPatt:parse()`");
        parser.log_current_token(false);

        Ok(GroupedPatt { inner_pattern })
    }
}
