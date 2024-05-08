use crate::{
    ast::{Delimiter, Pattern, Separator, TuplePatt, TuplePattElements},
    error::ErrorsEmitted,
    parser::Parser,
    token::Token,
};

impl TuplePatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let tuple_patt_elements = parse_tuple_patt_elements(parser)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_token("`)`");
            parser.log_unmatched_delimiter(&open_paren);
            Err(ErrorsEmitted)
        }?;

        let patt = TuplePatt {
            open_paren,
            tuple_patt_elements,
            close_paren,
        };

        Ok(Pattern::TuplePatt(patt))
    }
}

fn parse_tuple_patt_elements(parser: &mut Parser) -> Result<TuplePattElements, ErrorsEmitted> {
    let mut elements = Vec::new();
    let mut final_element_opt = None::<Box<Pattern>>;

    while !matches!(
        parser.current_token(),
        Some(Token::RParen { .. } | Token::EOF)
    ) {
        let element = parser.parse_pattern()?;

        if let Some(Token::Comma { .. }) = parser.current_token() {
            elements.push((element, Separator::Comma));
            parser.next_token();
        } else if !matches!(parser.current_token(), Some(Token::RParen { .. })) {
            parser.log_unexpected_token("`,` or `)`");
        } else if matches!(parser.current_token(), Some(Token::RParen { .. })) {
            final_element_opt = Some(Box::new(element));
            break;
        }
    }

    Ok(TuplePattElements {
        elements,
        final_element_opt,
    })
}
