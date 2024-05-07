use crate::{
    ast::{BlockExpr, Delimiter, Expression, InnerAttr, Statement},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{collection, ParseConstruct, Parser},
    token::Token,
};

impl ParseConstruct for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg("entering `BlockExpr::parse()`".to_string()),
        );
        parser.log_current_token(false);

        let attributes_opt = collection::get_attributes(parser, InnerAttr::inner_attr);

        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LBrace)
            // TODO: handle `None` case (`UnexpectedEndOfInput`)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let statements_opt = parse_statements(parser)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(&open_brace);
            Err(ErrorsEmitted)
        }?;

        let expr = BlockExpr {
            attributes_opt,
            open_brace,
            statements_opt,
            close_brace,
        };

        parser.logger.log(
            LogLevel::Debug,
            LogMsg("exiting `BlockExpr::parse()`".to_string()),
        );
        parser.log_current_token(false);
        Ok(Expression::Block(expr))
    }
}

fn parse_statements(parser: &mut Parser) -> Result<Option<Vec<Statement>>, ErrorsEmitted> {
    let mut statements: Vec<Statement> = Vec::new();

    while !matches!(
        parser.current_token(),
        Some(Token::RBrace { .. } | Token::EOF)
    ) {
        let statement = parser.parse_statement()?;
        statements.push(statement);

        if let Some(Token::Semicolon { .. }) = parser.current_token() {
            parser.next_token();
        }
    }

    match statements.is_empty() {
        true => Ok(None),
        false => Ok(Some(statements)),
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_block_expr() -> Result<(), ()> {
        let input = r#"
        #![unsafe]
        {
            x + 2;
            y
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
