use crate::{
    ast::{ArrayExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl ArrayExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let open_bracket = if let Some(Token::LBracket { .. }) = parser.peek_current() {
            Ok(Delimiter::LBracket)
        } else {
            parser.log_unexpected_token(TokenType::LBracket);
            Err(ErrorsEmitted)
        }?;

        parser.consume_token();

        let mut elements: Vec<Expression> = Vec::new();

        while !matches!(
            parser.peek_current(),
            Some(Token::RBracket { .. } | Token::EOF)
        ) {
            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            if let Some(Token::Comma { .. }) = parser.peek_current() {
                parser.consume_token();
            } else if !matches!(
                parser.peek_current(),
                Some(Token::RBracket { .. } | Token::EOF)
            ) {
                parser.log_unexpected_str("`,` or `}`");
                return Err(ErrorsEmitted);
            }
        }

        let close_bracket = parser.expect_delimiter(TokenType::RBracket)?;

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

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]

    fn parse_array_expr_with_elements() -> Result<(), ()> {
        let input = r#"[1, 2, 3, 4]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
