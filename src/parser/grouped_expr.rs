use crate::{
    ast::{Delimiter, Expression, GroupedExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{test_utils::log_token, Parser, Precedence};

impl GroupedExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        log_token(parser, "enter `GroupedExpr::parse()`", true);

        let open_paren = if let Some(Token::LParen { .. }) = parser.peek_current() {
            parser.consume_token();
            log_token(parser, "consume token", false);
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let inner_expression = parser.parse_expression(Precedence::Lowest)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = GroupedExpr {
            open_paren: Delimiter::LParen,
            expression: Box::new(inner_expression),
            close_paren,
        };

        log_token(parser, "exit `GroupedExpr::parse()`", true);

        Ok(Expression::Grouped(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_grouped_expr() -> Result<(), ()> {
        let input = r#"(x + 2)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
