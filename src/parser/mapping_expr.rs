use crate::{
    ast::{Delimiter, Expression, MappingExpr, MappingPair},
    error::ErrorsEmitted,
    token::Token,
};

use super::{collection, parse::ParseConstruct, Parser, Precedence};

impl ParseConstruct for MappingExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let pairs_opt = collection::get_collection(parser, parse_mapping_pair, Delimiter::RBrace)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(open_brace.clone());
            Err(ErrorsEmitted)
        }?;

        let expr = MappingExpr {
            open_brace,
            pairs_opt,
            close_brace,
        };

        Ok(Expression::Mapping(expr))
    }
}

fn parse_mapping_pair(parser: &mut Parser) -> Result<MappingPair, ErrorsEmitted> {
    let key = parser.parse_expression(Precedence::Lowest)?;

    if let Some(Token::Colon { .. }) = parser.current_token() {
        parser.next_token();
    } else {
        parser.log_unexpected_token("`:`");
    }

    let value = parser.parse_expression(Precedence::Lowest)?;

    let pair = MappingPair { key, value };

    Ok(pair)
}
