use crate::{
    ast::{Delimiter, GroupedExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl GroupedExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<GroupedExpr, ErrorsEmitted> {
        let expression = parser.parse_expression(Precedence::Lowest)?;

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(GroupedExpr {
            open_paren: Delimiter::LParen,
            expression: Box::new(expression),
            close_paren: close_paren?,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn test_grouped_expr() -> Result<(), ()> {
        let input = r#"(x + 2)"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
