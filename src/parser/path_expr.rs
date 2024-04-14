use crate::{
    ast::{Identifier, PathExpr, PathPrefix},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl PathExpr {
    pub(crate) fn parse(parser: &mut Parser, root: PathPrefix) -> Result<PathExpr, ErrorsEmitted> {
        let mut tree: Vec<Identifier> = Vec::new();

        if let Some(Token::DblColon { .. }) = parser.consume_token() {
            while let Some(Token::Identifier { name, .. }) = parser.consume_token() {
                tree.push(Identifier(name));

                if !parser.is_expected_token(&Token::DblColon {
                    punc: "::".to_string(),
                    span: parser.stream.span(),
                }) {
                    parser.consume_token();
                } else {
                    break;
                }
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
    use crate::parser::test_utils;

    #[test]
    fn parse_path_expr() -> Result<(), ()> {
        let input = r#"package::module::Object"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
