use crate::{
    ast::{GroupedPatt, Pattern, TuplePatt, TuplePattElements},
    error::ErrorsEmitted,
    log_trace,
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for GroupedPatt {
    fn parse_patt(parser: &mut Parser) -> Result<GroupedPatt, ErrorsEmitted> {
        log_trace!(parser.logger, "entering `GroupedPatt::parse()`");
        parser.log_current_token(false);

        parser.expect_open_paren()?;

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

        let _ = parser.get_parenthesized_item_span(None)?;

        log_trace!(parser.logger, "exiting `GroupedPatt:parse()`");
        parser.log_current_token(false);

        Ok(GroupedPatt { inner_pattern })
    }
}
