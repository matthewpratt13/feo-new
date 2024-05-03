use crate::{
    ast::{Delimiter, Expression, OuterAttr, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use super::{item::ParseAssociatedItem, Parser, Precedence};

pub(crate) fn get_collection<T>(
    parser: &mut Parser,
    f: fn(&mut Parser) -> Result<T, ErrorsEmitted>,
    close_delimiter: Delimiter,
) -> Result<Option<Vec<T>>, ErrorsEmitted> {
    let mut collection: Vec<T> = Vec::new();

    match close_delimiter {
        Delimiter::RParen => {
            while !matches!(
                parser.current_token(),
                Some(Token::RParen { .. } | Token::EOF),
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
        }

        Delimiter::RBrace => {
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
        }

        Delimiter::Pipe => {
            while !matches!(
                parser.current_token(),
                Some(Token::Pipe { .. } | Token::EOF),
            ) {
                let item = f(parser)?;
                collection.push(item);

                if let Some(Token::Comma { .. }) = parser.current_token() {
                    parser.next_token();
                } else if let Some(Token::Pipe { .. }) = parser.current_token() {
                    parser.next_token();
                    break;
                } else if !matches!(
                    parser.current_token(),
                    Some(Token::Pipe { .. } | Token::EOF)
                ) {
                    parser.log_unexpected_str("`,` or `|`");
                    return Err(ErrorsEmitted);
                }
            }
        }

        _ => {
            parser.log_unexpected_str("closing delimiter type");
            return Err(ErrorsEmitted);
        }
    }

    match collection.is_empty() {
        true => Ok(None),
        false => Ok(Some(collection)),
    }
}

pub(crate) fn get_expressions(
    parser: &mut Parser,
    precedence: Precedence,
    close_delimiter: Delimiter,
) -> Result<Option<Vec<Expression>>, ErrorsEmitted> {
    let mut expressions: Vec<Expression> = Vec::new();

    match close_delimiter {
        Delimiter::RParen => {
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
        }
        Delimiter::RBracket => {
            while !matches!(
                parser.current_token(),
                Some(Token::RBracket { .. } | Token::EOF),
            ) {
                let expr = parser.parse_expression(precedence)?;

                expressions.push(expr);

                if let Some(Token::Comma { .. }) = parser.current_token() {
                    parser.next_token();
                } else if !matches!(
                    parser.current_token(),
                    Some(Token::RBracket { .. } | Token::EOF)
                ) {
                    parser.log_unexpected_str("`,` or `]`");
                    return Err(ErrorsEmitted);
                }
            }
        }

        _ => {
            parser.log_unexpected_str("closing delimiter type");
            return Err(ErrorsEmitted);
        }
    }

    match expressions.is_empty() {
        true => Ok(None),
        false => Ok(Some(expressions)),
    }
}

pub(crate) fn get_associated_items<T: ParseAssociatedItem>(
    parser: &mut Parser,
) -> Result<Option<Vec<T>>, ErrorsEmitted> {
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

    match items.is_empty() {
        true => Ok(None),
        false => Ok(Some(items)),
    }
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
