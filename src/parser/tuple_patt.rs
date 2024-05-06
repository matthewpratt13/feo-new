use crate::{
    ast::{Delimiter, Pattern, Separator, TuplePatt, TuplePattElements},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl TuplePatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        let mut elements = Vec::new();

        let mut final_element_opt = None::<Box<Pattern>>;

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let tuple_patt_elements =
            parse_tuple_patt_elements(parser, &mut elements, &mut final_element_opt)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_token("`)`");
            parser.log_unmatched_delimiter(open_paren.clone());
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

fn parse_tuple_patt_elements(
    parser: &mut Parser,
    elements: &mut Vec<(Pattern, Separator)>,
    final_element_opt: &mut Option<Box<Pattern>>,
) -> Result<TuplePattElements, ErrorsEmitted> {
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
            *final_element_opt = Some(Box::new(element));
            break;
        }
    }
    let tuple_elements = TuplePattElements {
        elements: elements.clone(),
        final_element_opt: final_element_opt.clone(),
    };
    Ok(tuple_elements)
}
