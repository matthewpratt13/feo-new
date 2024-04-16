use crate::{
    ast::{Delimiter, GroupedExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl GroupedExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<GroupedExpr, ErrorsEmitted> {
        println!("ENTER `GroupedExpr::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let expression = parser.parse_expression(Precedence::Lowest)?;

        println!(
            "CURRENT TOKEN AFTER INNER EXPRESSION: {:?}",
            parser.peek_current()
        );

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        println!("EXIT `GroupedExpr::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

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
    fn parse_grouped_expr() -> Result<(), ()> {
        let input = r#"(x + 2)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
