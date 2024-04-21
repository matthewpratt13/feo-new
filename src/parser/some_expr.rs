use crate::{ast::SomeExpr, error::ErrorsEmitted, token::Token};

use super::{Parser, Precedence};

impl SomeExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<SomeExpr, ErrorsEmitted> {
        let kw_some = parser.expect_keyword(Token::Some {
            name: "Some".to_string(),
            span: parser.stream.span(),
        })?;

        let _ = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let _ = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        Ok(SomeExpr {
            kw_some,
            expression: Box::new(expression),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_some_expr() -> Result<(), ()> {
        let input = r#"Some(SomeStruct { foo: "bar", baz: -10 });"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_none_expr() -> Result<(), ()> {
        let input = r#"None"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
