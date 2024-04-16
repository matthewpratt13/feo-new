use crate::{
    ast::{BlockExpr, GroupedExpr, WhileExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl WhileExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<WhileExpr, ErrorsEmitted> {
        let kw_while = parser.expect_keyword(Token::While {
            name: "while".to_string(),
            span: parser.stream.span(),
        })?;

        let _ = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        });

        let condition = GroupedExpr::parse(parser)?;

        let block = BlockExpr::parse(parser)?;

        Ok(WhileExpr {
            kw_while,
            condition: Box::new(condition),
            block,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_while_expr() -> Result<(), ()> {
        let input = r#"
        while (x < 5) {
            x += 1
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
