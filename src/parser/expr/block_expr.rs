use crate::{
    ast::{BlockExpr, Delimiter, InnerAttr, Statement},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{collection, ParseConstructExpr, Parser},
    token::Token,
};

impl ParseConstructExpr for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        ////////////////////////////////////////////////////////////////////////////////
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `BlockExpr::parse()`"),
        );
        parser.log_current_token(false);
        ////////////////////////////////////////////////////////////////////////////////

        let attributes_opt = collection::get_attributes(parser, InnerAttr::inner_attr);

        let open_brace = match parser.current_token() {
            Some(Token::LBrace { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LBrace { position })
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let statements_opt = parse_statements(parser)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                parser.next_token();

                let expr = BlockExpr {
                    attributes_opt,
                    statements_opt,
                };

                ////////////////////////////////////////////////////////////////////////////////
                parser.logger.log(
                    LogLevel::Debug,
                    LogMsg::from("exiting `BlockExpr::parse()`"),
                );
                parser.log_current_token(false);
                ////////////////////////////////////////////////////////////////////////////////

                Ok(expr)
            }

            _ => {
                parser.log_unmatched_delimiter(&open_brace);
                Err(ErrorsEmitted)
            }
        }
    }
}

fn parse_statements(parser: &mut Parser) -> Result<Option<Vec<Statement>>, ErrorsEmitted> {
    let mut statements: Vec<Statement> = Vec::new();

    ////////////////////////////////////////////////////////////////////////////////
    parser.logger.log(
        LogLevel::Debug,
        LogMsg::from("entering `parse_statements()`"),
    );
    parser.log_current_token(false);
    ////////////////////////////////////////////////////////////////////////////////

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let statement = parser.parse_statement()?;
        statements.push(statement);
    }

    ////////////////////////////////////////////////////////////////////////////////
    parser.logger.log(
        LogLevel::Debug,
        LogMsg::from("exiting `parse_statements()`"),
    );
    parser.logger.log(
        LogLevel::Debug,
        LogMsg::from(format!("statements.is_empty(): {}", statements.is_empty())),
    );
    parser.log_current_token(false);
    ////////////////////////////////////////////////////////////////////////////////

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
