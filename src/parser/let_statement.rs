use core::fmt;

use crate::{
    ast::{IdentifierPatt, Keyword, LetStmt, Statement, Type},
    error::ErrorsEmitted,
    span::Spanned,
    token::{Token, TokenType},
};

use super::{ParsePattern, ParseStatement, Parser, Precedence};

impl ParseStatement for LetStmt {
    fn parse_statement(parser: &mut Parser) -> Result<Statement, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_let = if let Some(Token::Let { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Let)
        } else {
            parser.emit_unexpected_token(&TokenType::Let.to_string());
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

                Ok(Some(value))
            } else {
                parser.emit_missing_node("expr", "value");
                parser.next_token();
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
                parser.emit_unexpected_eoi();
                parser.warn_missing_token(&TokenType::Semicolon.to_string());
                Err(ErrorsEmitted)
            }
            _ => {
                parser.emit_unexpected_token(&TokenType::Semicolon.to_string());
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
