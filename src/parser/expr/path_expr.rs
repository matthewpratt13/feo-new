use crate::{
    ast::{Expression, Identifier, PathExpr, PathPrefix, Separator},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::Parser,
    token::Token,
};

impl PathExpr {
    pub(crate) fn parse(parser: &mut Parser, root: PathPrefix) -> Result<Expression, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `PathExpr::parse()`"),
        );
        parser.log_current_token(false);

        let mut tree: Vec<Identifier> = Vec::new();

        while let Some(Token::DblColon { .. }) = parser.current_token() {
            if let Some(Token::Identifier { name, .. }) = parser.peek_ahead_by(1) {
                parser.next_token();
                parser.next_token();

                tree.push(Identifier(name));
            } else {
                break;
            }
        }

        let wildcard_opt = if let Some(Token::ColonColonAsterisk { .. }) = parser.current_token() {
            parser.next_token();
            Some(Separator::ColonColonAsterisk)
        } else {
            None
        };

        let expr = PathExpr {
            root,
            tree_opt: {
                match tree.is_empty() {
                    true => None,
                    false => Some(tree),
                }
            },
            wildcard_opt,
        };

        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("exiting `PathExpr::parse()`"),
        );
        parser.log_current_token(false);

        Ok(Expression::Path(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_path_expr_identifier() -> Result<(), ()> {
        let input = r#"foo_bar"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_path_expr_standard() -> Result<(), ()> {
        let input = r#"package::some_module::SomeObject"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_path_expr_wildcard() -> Result<(), ()> {
        let input = r#"self::some_module::*"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
