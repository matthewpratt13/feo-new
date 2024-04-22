use crate::{
    ast::{Delimiter, SomeExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl SomeExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<SomeExpr, ErrorsEmitted> {
        let kw_some = parser.expect_keyword(Token::Some {
            name: "Some".to_string(),
            span: parser.stream.span(),
        })?;

        if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token("`(`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        if let Some(Token::RParen { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RParen)
        } else {
            parser.log_missing_delimiter(')');
            Err(ErrorsEmitted(()))
        }?;

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
