use crate::{
    ast::{Delimiter, Expression, GroupedExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{parse::ParseConstruct, test_utils::log_token, Parser, Precedence};

impl ParseConstruct for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        log_token(parser, "enter `GroupedExpr::parse()`", true);

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            log_token(parser, "consume token", false);
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = GroupedExpr {
            open_paren,
            expression: Box::new(expression),
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
