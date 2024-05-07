use crate::{
    ast::{Delimiter, Expression, MappingExpr, MappingPair},
    error::ErrorsEmitted,
    parser::{collection, ParseConstruct, Parser, Precedence},
    token::Token,
};

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
            parser.log_unmatched_delimiter(&open_brace);
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
    let key = parser.parse_pattern()?;

    match parser.current_token() {
        Some(Token::Colon { .. }) => {
            let _ = parser.next_token();
        }
        Some(_) => parser.log_unexpected_token("`:`"),
        None => parser.log_missing_token("`:`"),
    }

    let value = parser.parse_expression(Precedence::Lowest)?;

    let pair = MappingPair { key, value };

    Ok(pair)
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_mapping_expr_empty() -> Result<(), ()> {
        let input = r#"{}"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]

    fn parse_mapping_expr_with_elements() -> Result<(), ()> {
        let input = r#"{x: 2, y: true, z: foo}"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
