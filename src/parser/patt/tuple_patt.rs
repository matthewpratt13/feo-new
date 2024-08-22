use crate::{
    ast::{Delimiter, Pattern, TuplePatt, TuplePattElements},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    span::Position,
    token::Token,
};

impl ParsePattern for TuplePatt {
    fn parse_patt(parser: &mut Parser) -> Result<TuplePatt, ErrorsEmitted> {
        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            let position = Position::new(parser.current, &parser.stream.span().input());
            parser.next_token();
            Ok(Delimiter::LParen { position })
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let tuple_patt_elements = parse_tuple_patt_elements(parser)?;

        let _ = parser.get_parenthesized_item_span(None, &open_paren)?;

        Ok(TuplePatt {
            tuple_patt_elements,
        })
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
            elements.push(element);
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
