use crate::{
    ast::{Delimiter, Expression, OuterAttr, Visibility},
    error::ErrorsEmitted,
    log_debug, log_trace,
    token::{Token, TokenType},
};

use super::{ParseAssociatedItem, Parser, Precedence};

/// Helper function that collects a generic element into a vector based on the input function.
/// Takes in a `Delimiter` (for reusability) to distinguish the outer limits of the collection.
/// Replaces the same iterative code in the respective parsing functions.
pub(crate) fn get_collection<T>(
    parser: &mut Parser,
    f: fn(&mut Parser) -> Result<T, ErrorsEmitted>,
    open_delimiter: &Delimiter,
) -> Result<Option<Vec<T>>, ErrorsEmitted> {
    let mut collection: Vec<T> = Vec::new();

    match open_delimiter {
        Delimiter::LParen { .. } => {
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
                    parser.emit_unexpected_token(&format!(
                        "{} or {}",
                        TokenType::Comma,
                        TokenType::RParen
                    ));
                    return Err(ErrorsEmitted);
                }
            }
        }

        Delimiter::LBrace { .. } => {
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
                    parser.emit_unexpected_token(&format!(
                        "{} or {}",
                        TokenType::Comma,
                        TokenType::RBrace
                    ));
                    return Err(ErrorsEmitted);
                }
            }
        }

        Delimiter::Pipe { .. } => {
            while !matches!(
                parser.current_token(),
                Some(Token::Pipe { .. } | Token::EOF),
            ) {
                let item = f(parser)?;
                collection.push(item);

                if let Some(Token::Comma { .. }) = parser.current_token() {
                    parser.next_token();
                } else if let Some(Token::Pipe { .. }) = parser.current_token() {
                    break;
                } else if !matches!(
                    parser.current_token(),
                    Some(Token::Pipe { .. } | Token::EOF)
                ) {
                    parser.emit_unexpected_token(&format!(
                        "{} or {}",
                        TokenType::Comma,
                        TokenType::Pipe
                    ));
                    return Err(ErrorsEmitted);
                }
            }
        }

        Delimiter::LAngleBracket { .. } => {
            while !matches!(
                parser.current_token(),
                Some(Token::GreaterThan { .. } | Token::EOF),
            ) {
                let item = f(parser)?;
                collection.push(item);

                if let Some(Token::Comma { .. }) = parser.current_token() {
                    parser.next_token();
                } else if let Some(Token::GreaterThan { .. }) = parser.current_token() {
                    break;
                } else if !matches!(
                    parser.current_token(),
                    Some(Token::GreaterThan { .. } | Token::EOF)
                ) {
                    parser.emit_unexpected_token(&format!(
                        "{} or {}",
                        TokenType::Comma,
                        TokenType::GreaterThan
                    ));
                    return Err(ErrorsEmitted);
                }
            }
        }

        _ => {
            parser.emit_unexpected_token("closing delimiter type");
            return Err(ErrorsEmitted);
        }
    }

    match collection.is_empty() {
        true => Ok(None),
        false => Ok(Some(collection)),
    }
}

/// Helper function that collects expressions into a vector.
/// Takes in a `Delimiter` (for reusability) to distinguish the outer limits of the collection.
/// Replaces the same iterative code in the respective expression parsing functions.
pub(crate) fn get_expressions(
    parser: &mut Parser,
    precedence: Precedence,
    open_delimiter: &Delimiter,
) -> Result<Option<Vec<Expression>>, ErrorsEmitted> {
    let mut expressions: Vec<Expression> = Vec::new();

    log_trace!(parser.logger, "entering `get_expressions()` …");

    parser.log_current_token(false);

    match open_delimiter {
        Delimiter::LParen { .. } => {
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
                    parser.emit_unexpected_token(&format!(
                        "{} or {}",
                        TokenType::Comma,
                        TokenType::RParen
                    ));
                    return Err(ErrorsEmitted);
                }
            }
        }
        Delimiter::LBracket { .. } => {
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
                    parser.emit_unexpected_token(&format!(
                        "{} or {}",
                        TokenType::Comma,
                        TokenType::RBracket
                    ));
                    return Err(ErrorsEmitted);
                }
            }
        }

        _ => {
            parser.emit_unexpected_token("closing delimiter type");
            return Err(ErrorsEmitted);
        }
    }

    log_trace!(parser.logger, "exiting `get_expressions()` …");
    log_debug!(
        parser.logger,
        "expressions.is_empty(): {}",
        expressions.is_empty()
    );

    parser.log_current_token(false);

    match expressions.is_empty() {
        true => Ok(None),
        false => Ok(Some(expressions)),
    }
}

/// Helper function that collects associated items (items that implement `ParseAssociatedItem`)
/// into a vector.
/// Replaces the same iterative code in the respective item parsing functions.
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

/// Helper function that parses item attributes into a vector of generic attributes.
/// Replaces the same iterative code in the respective item parsing functions.
pub(crate) fn get_attributes<T>(
    parser: &mut Parser,
    f: fn(&Parser) -> Option<T>,
) -> Option<Vec<T>> {
    let mut attributes = Vec::new();

    log_trace!(parser.logger, "entering `get_attributes()` …");
    parser.log_current_token(false);

    while let Some(a) = f(parser) {
        attributes.push(a);
        parser.next_token();
    }

    log_trace!(parser.logger, "exiting `get_attributes()` …");
    log_debug!(
        parser.logger,
        "attributes.is_empty(): {}",
        attributes.is_empty()
    );

    parser.log_current_token(false);

    match attributes.is_empty() {
        true => None,
        false => Some(attributes),
    }
}
