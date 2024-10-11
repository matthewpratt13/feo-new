use core::fmt;

use crate::{
    ast::{BlockExpr, InnerAttr, Statement},
    error::ErrorsEmitted,
    log_debug, log_error, log_trace,
    parser::{get_attributes, ParseConstructExpr, Parser},
    token::Token,
};

impl ParseConstructExpr for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        log_trace!(parser.logger, "entering `BlockExpr::parse()`");
        parser.log_current_token(false);

        let first_token = parser.current_token().cloned();

        let attributes_opt = get_attributes(parser, InnerAttr::inner_attr);

        parser.expect_open_brace()?;

        let statements_opt = parse_statements(parser)?;

        let span = parser.get_braced_item_span(first_token.as_ref())?;

        log_trace!(parser.logger, "exiting `BlockExpr::parse()`");
        parser.log_current_token(false);

        Ok(BlockExpr {
            attributes_opt,
            statements_opt,
            span,
        })
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

    log_trace!(parser.logger, "entering `parse_statements()`");
    parser.log_current_token(false);

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let statement = parser.parse_statement().map_err(|errors| {
            for err in errors {
                log_error!(parser.logger, "{err}");
            }

            ErrorsEmitted
        })?;

        statements.push(statement);
    }

    log_trace!(parser.logger, "exiting `parse_statements()`");
    log_debug!(
        parser.logger,
        "statements.is_empty(): {}",
        &statements.is_empty()
    );
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
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
