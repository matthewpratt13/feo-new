use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, IndexExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl IndexExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        array: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let array = AssigneeExpr::try_from(array).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_bracket = if let Some(Token::LBracket { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LBracket)
        } else {
            parser.log_unexpected_token(TokenType::LBracket);
            Err(ErrorsEmitted)
        }?;

        let index = parser.parse_expression(Precedence::Lowest)?;

        let close_bracket = if let Some(Token::RBracket { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::RBracket)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBracket,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = IndexExpr {
            array: Box::new(array),
            open_bracket,
            index: Box::new(index),
            close_bracket,
        };

        Ok(Expression::Index(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_index_expr_uint() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_index_expr_identifier() -> Result<(), ()> {
        let input = r#"array[index]"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
