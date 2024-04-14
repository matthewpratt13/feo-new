use crate::{
    ast::{Delimiter, Expression, IndexExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl IndexExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        array: Expression,
    ) -> Result<IndexExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        let index = if let Some(Token::UIntLiteral { value, .. }) = token {
            Ok(value)
        } else if let Some(t) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "unsigned integer".to_string(),
                found: t,
            });
            Err(ErrorsEmitted(()))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        };

        let close_bracket = parser.expect_delimiter(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(IndexExpr {
            array: Box::new(array),
            open_bracket: Delimiter::LBracket,
            index: index?,
            close_bracket: close_bracket?,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn test_index_expr() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
