use super::{ParsePattern, ParseStatement, Parser, Precedence};

use crate::{
    ast::{IdentifierPatt, Keyword, LetStmt, Statement, Type},
    error::ErrorsEmitted,
    span::Spanned,
    token::Token,
};

use core::fmt;

impl ParseStatement for LetStmt {
    fn parse_statement(parser: &mut Parser) -> Result<Statement, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_let = if let Some(Token::Let { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Let)
        } else {
            parser.log_unexpected_token("`let`");
            Err(ErrorsEmitted)
        }?;

        let assignee = IdentifierPatt::parse_patt(parser)?;

        let type_ann_opt = if let Some(Token::Colon { .. }) = parser.current_token() {
            parser.next_token();
            Some(Type::parse(parser)?)
        } else {
            None
        };

        let value_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();

            if parser.current_token().is_some() {
                let value = parser.parse_expression(Precedence::Lowest)?;
                println!("value: {value:?}");

                Ok(Some(value))
            } else {
                parser.log_missing("expr", "value");
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        match parser.current_token() {
            Some(Token::Semicolon { .. }) => {
                let span = parser.get_span(
                    &first_token.unwrap().span(),
                    &value_opt.as_ref().unwrap().span(),
                );

                parser.next_token();
                let stmt = LetStmt {
                    kw_let,
                    assignee,
                    type_ann_opt,
                    value_opt,
                    span,
                };

                Ok(Statement::Let(stmt))
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`;`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`;`");
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for LetStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LetStmt")
            .field("assignee", &self.assignee)
            .field("type_ann_opt", &self.type_ann_opt)
            .field("value_opt", &self.value_opt)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_let_stmt() -> Result<(), ()> {
        let input = r#"let x: str = "hello world";"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
