use crate::{
    ast::{Expression, GroupedExpr, Keyword, SomeExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::{parse::ParseConstruct, Parser};

impl ParseConstruct for SomeExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_some = if let Some(Token::Some { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Some)
        } else {
            parser.log_unexpected_token("`Some`");
            Err(ErrorsEmitted)
        }?;

        let expression = if let Some(Token::LParen { .. }) = parser.current_token() {
            Ok(Box::new(GroupedExpr::parse(parser)?))
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let expr = SomeExpr {
            kw_some,
            expression,
        };

        Ok(Expression::SomeExpr(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_some_expr() -> Result<(), ()> {
        let input = r#"Some(foo)"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_none_expr() -> Result<(), ()> {
        let input = r#"None"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
