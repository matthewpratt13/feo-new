use crate::{
    ast::{Delimiter, MappingPatt, Pattern},
    error::ErrorsEmitted,
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for MappingPatt {
    fn parse_patt(parser: &mut Parser) -> Result<MappingPatt, ErrorsEmitted> {
        let mut pairs: Vec<(Box<Pattern>, Box<Pattern>)> = Vec::new();

        let first_token = parser.current_token().cloned();

        let open_brace = match &first_token {
            Some(Token::LBrace { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LBrace { position })
            }
            _ => Err(parser.log_unexpected_token("`{`")),
        }?;

        while !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF),
        ) {
            let pair = parse_mapping_patt_pair(parser)?;
            pairs.push(pair);

            if let Some(Token::Comma { .. }) = parser.current_token() {
                parser.next_token();
            } else if !matches!(
                parser.current_token(),
                Some(Token::RBrace { .. } | Token::EOF)
            ) {
                parser.log_unexpected_token("`,` or `}`");
                return Err(ErrorsEmitted);
            }
        }

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                parser.next_token();
                Ok(MappingPatt { pairs })
            }
            _ => Err(parser.log_unmatched_delimiter(&open_brace)),
        }
    }
}

fn parse_mapping_patt_pair(
    parser: &mut Parser,
) -> Result<(Box<Pattern>, Box<Pattern>), ErrorsEmitted> {
    let key = parser.parse_pattern()?;

    match parser.current_token() {
        Some(Token::Colon { .. }) => {
            parser.next_token();

            if let Some(Token::Comma { .. } | Token::RBrace { .. }) = parser.current_token() {
                parser.log_missing("patt", &format!("value for key: \"{}\"", &key));
                return Err(ErrorsEmitted);
            }
        }
        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            return Err(ErrorsEmitted);
        }
        _ => {
            parser.log_unexpected_token("`:`");
            return Err(ErrorsEmitted);
        }
    }

    let value = parser.parse_pattern()?;

    Ok((Box::new(key), Box::new(value)))
}
