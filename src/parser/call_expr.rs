use crate::{
    ast::{AssigneeExpr, CallExpr, Delimiter, Expression},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, parse::ParseOperation, Parser, Precedence};

impl ParseOperation for CallExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let callee = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_paren = if let Some(Token::LParen { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let args_opt = collection::get_expressions(parser, Precedence::Lowest, Delimiter::RParen)?;

        let close_paren = if let Some(Token::RParen { .. }) = parser.next_token() {
            Ok(Delimiter::RParen)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RParen,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = CallExpr {
            callee,
            open_paren,
            args_opt,
            close_paren,
        };

        Ok(Expression::Call(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_call_expr() -> Result<(), ()> {
        let input = r#"foo(b"bar", -10, x)"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
