use crate::{
    ast::{Expression, Identifier, PathExpr, PathRoot, SelfType},
    error::ErrorsEmitted,
    parser::{ParseSimpleExpr, Parser},
    span::Spanned,
    token::{Token, TokenType},
};

use core::fmt;

impl ParseSimpleExpr for PathExpr {
    fn parse(parser: &mut Parser) -> Result<PathExpr, ErrorsEmitted> {
        parser.logger.trace("entering `PathExpr::parse()`");
        parser.log_current_token(false);

        let mut tree: Vec<Identifier> = Vec::new();

        let first_token = parser.current_token().cloned();

        let path_root = match &first_token {
            Some(Token::Identifier { name, .. }) => {
                Ok(PathRoot::Identifier(Identifier::from(name)))
            }
            Some(Token::SelfKeyword { .. }) => Ok(PathRoot::SelfKeyword),
            Some(Token::SelfType { .. }) => Ok(PathRoot::SelfType(SelfType)),
            Some(Token::Lib { .. }) => Ok(PathRoot::Lib),
            Some(Token::Super { .. }) => Ok(PathRoot::Super),
            _ => {
                parser.log_unexpected_token(&format!(
                    "path root (identifier, {}, {}, {} or {})",
                    TokenType::Lib,
                    TokenType::Super,
                    TokenType::SelfKeyword,
                    TokenType::SelfType,
                ));
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        while let Some(Token::DblColon { .. }) = parser.current_token() {
            match parser.peek_ahead_by(1).cloned() {
                Some(Token::Identifier { name, .. }) => {
                    parser.next_token();
                    parser.next_token();

                    tree.push(Identifier::from(&name));
                }
                Some(Token::LBrace { .. }) => break,
                Some(Token::EOF) | None => {
                    parser.log_unexpected_eoi();
                    return Err(ErrorsEmitted);
                }
                _ => {
                    parser.log_unexpected_token("identifier");
                    return Err(ErrorsEmitted);
                }
            }
        }

        let last_token = parser.peek_behind_by(1);

        let span = parser.get_span(&first_token.unwrap().span(), &last_token.unwrap().span());

        parser.logger.trace("exiting `PathExpr::parse()`");
        parser.log_current_token(false);

        Ok(PathExpr {
            path_root,
            tree_opt: {
                match tree.is_empty() {
                    true => None,
                    false => Some(tree),
                }
            },
            span,
        })
    }
}

impl From<Expression> for PathExpr {
    fn from(value: Expression) -> Self {
        match value {
            Expression::Path(p) => p,
            e => PathExpr {
                path_root: PathRoot::Identifier(Identifier::from("")),
                tree_opt: None,
                span: e.span(),
            },
        }
    }
}

impl fmt::Display for PathExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut segments: Vec<String> = Vec::new();

        segments.push(self.path_root.to_string());

        if let Some(v) = &self.tree_opt {
            for i in v {
                segments.push(i.to_string());
            }
        }

        let full_path = segments.join("::");

        write!(f, "{}", full_path)
    }
}

impl fmt::Debug for PathExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PathExpr")
            .field("path_root", &self.path_root)
            .field("tree_opt", &self.tree_opt)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_path_expr_identifier() -> Result<(), ()> {
        let input = r#"foo_bar"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_path_expr_self_keyword() -> Result<(), ()> {
        let input = r#"self"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_path_expr_full() -> Result<(), ()> {
        let input = r#"lib::some_module::SomeObject"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_path_expr_super() -> Result<(), ()> {
        let input = r#"super::SOME_CONSTANT"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_path_expr_method() -> Result<(), ()> {
        let input = r#"Self::method"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
