use core::fmt;

use crate::{
    ast::{Keyword, ReturnExpr},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, Parser, Precedence},
    token::{Token, TokenType},
};

impl ParseConstructExpr for ReturnExpr {
    fn parse(parser: &mut Parser) -> Result<ReturnExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_return = if let Some(Token::Return { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Return)
        } else {
            parser.emit_unexpected_token(&TokenType::Return.to_string());
            Err(ErrorsEmitted)
        }?;

        let expression_opt = match parser.current_token() {
            Some(Token::RBrace { .. } | Token::Semicolon { .. } | Token::EOF) | None => None,
            _ => Some(Box::new(parser.parse_expression(Precedence::Lowest)?)),
        };

        let span = parser.get_span_by_token(&first_token.unwrap());

        if let Some(Token::Semicolon { .. }) = parser.current_token() {
            parser.next_token();
        }

        Ok(ReturnExpr {
            kw_return,
            expression_opt,
            span,
        })
    }
}

impl fmt::Debug for ReturnExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReturnExpr")
            .field("kw_return", &self.kw_return)
            .field("expression_opt", &self.expression_opt)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_return_expr() -> Result<(), ()> {
        let input = r#"return (x + 2)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
