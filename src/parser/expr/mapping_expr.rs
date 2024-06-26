use crate::{
    ast::{Delimiter, IdentifierPatt, MappingExpr, MappingPair},
    error::ErrorsEmitted,
    parser::{collection, ParseConstructExpr, ParsePattern, Parser, Precedence},
    token::Token,
};

impl ParseConstructExpr for MappingExpr {
    fn parse(parser: &mut Parser) -> Result<MappingExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let open_brace = match &first_token {
            Some(Token::LBrace { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LBrace { position })
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let pairs_opt = collection::get_collection(parser, parse_mapping_pair, &open_brace)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());

                parser.next_token();

                Ok(MappingExpr { pairs_opt, span })
            }
            _ => {
                parser.log_unmatched_delimiter(&open_brace);
                Err(ErrorsEmitted)
            }
        }
    }
}

fn parse_mapping_pair(parser: &mut Parser) -> Result<MappingPair, ErrorsEmitted> {
    let key = IdentifierPatt::parse_patt(parser)?;

    match parser.current_token() {
        Some(Token::Colon { .. }) => {
            parser.next_token();

            if let Some(Token::Comma { .. } | Token::RBrace { .. }) = parser.current_token() {
                parser.log_missing("expr", &format!("value for key: \"{}\"", &key));
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

    let value = Box::new(parser.parse_expression(Precedence::Lowest)?);

    Ok(MappingPair { key, value })
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_mapping_expr_empty() -> Result<(), ()> {
        let input = r#"{}"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]

    fn parse_mapping_expr_with_elements() -> Result<(), ()> {
        let input = r#"{x: 2, y: true, z: foo }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
