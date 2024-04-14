use crate::{
    ast::{Expression, Identifier, PathExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl PathExpr {
    pub(crate) fn parse(parser: &mut Parser, prefix: Expression) -> Result<PathExpr, ErrorsEmitted> {
        println!("ENTER `PathExpr::parse()`");

        let mut tree: Vec<Identifier> = Vec::new();

        let token = parser.consume_token();

        let root = match prefix {
            Expression::Path(p) => Ok(p.root),
            _ => {
                let token = token.ok_or({
                    parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
                    ErrorsEmitted(())
                })?;

                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "path expression prefix".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }?;

        while let Some(Token::DblColon { .. }) = parser.peek_current() {
            parser.consume_token();

            if let Some(Token::Identifier { name, .. }) = parser.peek_current() {
                tree.push(Identifier(name));
            } else {
                break;
            }
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if tree.is_empty() {
            Ok(PathExpr {
                root,
                tree_opt: None,
            })
        } else {
            Ok(PathExpr {
                root,
                tree_opt: Some(tree),
            })
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::test_utils;

    #[test]
    #[ignore]
    fn test_path_expr() -> Result<(), ()> {
        let input = r#"package::module::Object"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
