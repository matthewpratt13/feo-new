use crate::{
    ast::{Expression, OuterAttr, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use super::{item::ParseAssociatedItem, Parser, Precedence};

pub(crate) fn get_collection_braces_comma<T>(
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
pub(crate) fn get_collection_parens_comma<T>(
    parser: &mut Parser,
    f: fn(&mut Parser) -> Result<T, ErrorsEmitted>,
) -> Result<Vec<T>, ErrorsEmitted> {
    let mut collection: Vec<T> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RParen { .. } | Token::EOF)
    ) {
        let item = f(parser)?;

        collection.push(item);

        if let Some(Token::Comma { .. }) = parser.current_token() {
            parser.next_token();
        } else if !matches!(
            parser.current_token(),
            Some(Token::RParen { .. } | Token::EOF)
        ) {
            parser.log_unexpected_str("`,` or `)`");
            return Err(ErrorsEmitted);
        }
    }

    Ok(collection)
}

pub(crate) fn get_expressions(
    parser: &mut Parser,
    precedence: Precedence,
) -> Result<Vec<Expression>, ErrorsEmitted> {
    let mut expressions: Vec<Expression> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RParen { .. } | Token::EOF),
    ) {
        let expr = parser.parse_expression(precedence)?;

        expressions.push(expr);

        if let Some(Token::Comma { .. }) = parser.current_token() {
            parser.next_token();
        } else if !matches!(
            parser.current_token(),
            Some(Token::RParen { .. } | Token::EOF)
        ) {
            parser.log_unexpected_str("`,` or `)`");
            return Err(ErrorsEmitted);
        }
    }

    Ok(expressions)
}

pub(crate) fn get_associated_items<T: ParseAssociatedItem>(
    parser: &mut Parser,
) -> Result<Vec<T>, ErrorsEmitted> {
    let mut items: Vec<T> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let attributes_opt = get_attributes(parser, OuterAttr::outer_attr);

        let item_visibility = Visibility::visibility(parser)?;

        let item = T::parse(parser, attributes_opt, item_visibility)?;
        items.push(item);
    }

    Ok(items)
}

pub(crate) fn get_attributes<T>(
    parser: &mut Parser,
    f: fn(&Parser) -> Option<T>,
) -> Option<Vec<T>> {
    let mut attributes = Vec::new();

    while let Some(a) = f(parser) {
        attributes.push(a);
        parser.next_token();
    }

    match attributes.is_empty() {
        true => None,
        false => Some(attributes),
    }
}
