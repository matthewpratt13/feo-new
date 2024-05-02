use crate::{error::ErrorsEmitted, token::Token};

use super::Parser;

pub(crate) fn get_collection_braces<T>(
    parser: &mut Parser,
    f: fn(&mut Parser) -> Result<T, ErrorsEmitted>,
) -> Result<Vec<T>, ErrorsEmitted> {
    let mut collection: Vec<T> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF),
    ) {
        let item = f(parser)?;

        collection.push(item);

        if let Some(Token::Comma { .. }) = parser.current_token() {
            parser.next_token();
        } else if !matches!(
            parser.current_token(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            parser.log_unexpected_str("`,` or `}`");
            return Err(ErrorsEmitted);
        }
    }

    Ok(collection)
}
