use crate::{
    ast::{Pattern, TuplePatt, TuplePattElements},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::{Token, TokenType},
};

impl ParsePattern for TuplePatt {
    fn parse_patt(parser: &mut Parser) -> Result<TuplePatt, ErrorsEmitted> {
        parser.expect_open_paren()?;

        let tuple_patt_elements = parse_tuple_patt_elements(parser)?;

        let _ = parser.get_parenthesized_item_span(None)?;

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
            parser.log_unexpected_token(&format!("{} or {}", TokenType::Comma, TokenType::RParen));
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
