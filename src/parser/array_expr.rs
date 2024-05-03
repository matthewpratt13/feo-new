use crate::{
    ast::{ArrayExpr, Delimiter, Expression},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, Parser, Precedence};

impl ArrayExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let open_bracket = if let Some(Token::LBracket { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LBracket)
        } else {
            parser.log_unexpected_token(TokenType::LBracket);
            Err(ErrorsEmitted)
        }?;

        let elements =
            collection::get_expressions(parser, Precedence::Lowest, Delimiter::RBracket)?;

        let close_bracket = if let Some(Token::RBracket { .. }) = parser.next_token() {
            Ok(Delimiter::RBracket)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBracket,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = ArrayExpr {
            open_bracket,
            elements_opt: {
                if elements.is_empty() {
                    None
                } else {
                    Some(elements)
                }
            },
            close_bracket,
        };

        Ok(Expression::Array(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_array_expr_empty() -> Result<(), ()> {
        let input = r#"[]"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]

    fn parse_array_expr_with_elements() -> Result<(), ()> {
        let input = r#"[1, 2, 3, 4]"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
