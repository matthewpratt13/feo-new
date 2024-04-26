use crate::{
    ast::{Expression, Identifier, PathExpr, PathPrefix, Separator},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl PathExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        root: PathPrefix,
    ) -> Result<Expression, ErrorsEmitted> {
        println!("enter `PathExpr::parse()`");
        println!("current token: {:?}\n", parser.peek_current());

        let mut tree: Vec<Identifier> = Vec::new();

        while let Some(Token::DblColon { .. }) = parser.peek_current() {
            if let Some(Token::Identifier { name, .. }) = parser.peek_ahead_by(1) {
                parser.consume_token();
                parser.consume_token();

                tree.push(Identifier(name));
            } else {
                break;
            }
        }

        let wildcard_opt = if let Some(Token::ColonColonAsterisk { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Separator::ColonColonAsterisk)
        } else {
            None
        };

        let expr = PathExpr {
            root,
            tree_opt: {
                if tree.is_empty() {
                    None
                } else {
                    Some(tree)
                }
            },
            wildcard_opt,
        };

        Ok(Expression::Path(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_path_expr_identifier() -> Result<(), ()> {
        let input = r#"foo_bar"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_path_expr() -> Result<(), ()> {
        let input = r#"package::some_module::SomeObject"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_path_expr_wildcard() -> Result<(), ()> {
        let input = r#"self::some_module::*"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
