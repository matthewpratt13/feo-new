use crate::{
    ast::{Delimiter, Expression, IndexExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl IndexExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        array: Expression,
    ) -> Result<IndexExpr, ErrorsEmitted> {
        let index = parser.parse_primary()?;

        let close_bracket = parser.expect_delimiter(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        })?;

        Ok(IndexExpr {
            array: Box::new(array),
            open_bracket: Delimiter::LBracket,
            index: Box::new(index),
            close_bracket,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_index_expr_uint() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_index_expr_identifier() -> Result<(), ()> {
        let input = r#"array[index]"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
