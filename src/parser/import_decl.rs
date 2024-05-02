use crate::{
    ast::{
        Delimiter, Identifier, ImportDecl, ImportTree, Keyword, OuterAttr, PathExpr, PathPrefix,
        PathSegment, PathSubset, Separator, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, item::ParseDeclaration, Parser};

impl ParseDeclaration for ImportDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<ImportDecl, ErrorsEmitted> {
        let kw_import = parser.expect_keyword(TokenType::Import)?;

        let tree = ImportTree::parse(parser)?;

        parser.expect_separator(TokenType::Semicolon)?;

        Ok(ImportDecl {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_import,
            tree,
        })
    }
}

impl ImportTree {
    fn parse(parser: &mut Parser) -> Result<ImportTree, ErrorsEmitted> {
        let mut path_segments: Vec<PathSegment> = Vec::new();

        let first_segment = PathSegment::parse(parser)?;
        path_segments.push(first_segment);

        while let Some(Token::DblColon { .. }) = parser.current_token() {
            parser.next_token();

            let next_segment = PathSegment::parse(parser)?;
            path_segments.push(next_segment);
        }

        let wildcard_opt = if let Some(Token::ColonColonAsterisk { .. }) = parser.current_token() {
            parser.next_token();
            Some(Separator::ColonColonAsterisk)
        } else {
            None
        };

        let as_clause_opt = if let Some(Token::As { .. }) = parser.current_token() {
            let kw_as = Keyword::As;
            parser.next_token();

            let id = if let Some(Token::Identifier { name, .. }) = parser.current_token() {
                parser.next_token();
                Ok(Identifier(name))
            } else {
                parser.log_unexpected_str("identifier");
                Err(ErrorsEmitted)
            }?;

            Some((kw_as, id))
        } else {
            None
        };

        Ok(ImportTree {
            path_segments,
            wildcard_opt,
            as_clause_opt,
        })
    }
}

impl PathSegment {
    fn parse(parser: &mut Parser) -> Result<PathSegment, ErrorsEmitted> {
        let token = parser.next_token();

        let root = match token {
            Some(Token::Package { .. }) => PathExpr::parse(parser, PathPrefix::Package),
            Some(Token::Super { .. }) => PathExpr::parse(parser, PathPrefix::Super),
            Some(Token::SelfKeyword { .. }) => PathExpr::parse(parser, PathPrefix::SelfKeyword),
            Some(Token::Identifier { .. }) => PathExpr::parse(parser, PathPrefix::Package),
            _ => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "path prefix".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted)
            }
        }?;

        let subset_opt = if let Some(Token::LBrace { .. }) = parser.peek_ahead_by(1) {
            parser.next_token();
            Some(PathSubset::parse(parser)?)
        } else {
            None
        };

        Ok(PathSegment { root, subset_opt })
    }
}

impl PathSubset {
    fn parse(parser: &mut Parser) -> Result<PathSubset, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let trees = collection::get_collection_braces_comma(parser, ImportTree::parse)?;

        let close_brace = parser.expect_delimiter(TokenType::RBrace)?;

        Ok(PathSubset {
            open_brace,
            trees,
            close_brace,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_import_decl_simple() -> Result<(), ()> {
        let input = r#"pub import package::some_module::SomeObject;"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_import_decl_nested() -> Result<(), ()> {
        let input = r#"
        pub import package::some_module::{
            SomeObject,
            inner_module::{ 
                SomeInnerObject,
                AnotherInnerObject
            },
            another_inner_module::*,
            SOME_CONSTANT,
        };"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_import_decl_with_as_clause() -> Result<(), ()> {
        let input = r#"pub import package::some_module::SomeObject as AnotherObject;"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
