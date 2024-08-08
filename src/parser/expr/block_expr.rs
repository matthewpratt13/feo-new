use crate::{
    ast::{BlockExpr, Delimiter, InnerAttr, Statement},
    error::ErrorsEmitted,
    parser::{collection, ParseConstructExpr, Parser},
    token::Token,
};

use core::fmt;

impl ParseConstructExpr for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        parser.logger.debug("entering `BlockExpr::parse()`");
        parser.log_current_token(false);

        let first_token = parser.current_token().cloned();

        let attributes_opt = collection::get_attributes(parser, InnerAttr::inner_attr);

        let open_brace = match parser.current_token() {
            Some(Token::LBrace { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LBrace { position })
            }
            Some(Token::EOF) | None => Err(parser.log_unexpected_eoi()),
            _ => Err(parser.log_unexpected_token("`{`")),
        }?;

        let statements_opt = parse_statements(parser)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());
                parser.next_token();

                let expr = BlockExpr {
                    attributes_opt,
                    statements_opt,
                    span,
                };

                parser.logger.debug("exiting `BlockExpr::parse()`");
                parser.log_current_token(false);

                Ok(expr)
            }

            _ => Err(parser.log_unmatched_delimiter(&open_brace)),
        }
    }
}

impl fmt::Debug for BlockExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BlockExpr")
            .field("attributes_opt", &self.attributes_opt)
            .field("statements_opt", &self.statements_opt)
            .finish()
    }
}

fn parse_statements(parser: &mut Parser) -> Result<Option<Vec<Statement>>, ErrorsEmitted> {
    let mut statements: Vec<Statement> = Vec::new();

    parser.logger.debug("entering `parse_statements()`");
    parser.log_current_token(false);

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let statement = parser.parse_statement().map_err(|errors| {
            for err in errors {
                parser.logger.error(&err.to_string());
            }

            ErrorsEmitted
        })?;

        statements.push(statement);
    }

    parser.logger.debug("exiting `parse_statements()`");
    parser.logger.debug(&format!(
        "statements.is_empty(): {}",
        &statements.is_empty()
    ));
    parser.log_current_token(false);

    match statements.is_empty() {
        true => Ok(None),
        false => Ok(Some(statements)),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_block_expr() -> Result<(), ()> {
        let input = r#" 
        #![unsafe] 
        {
            x + 5;
            y
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
