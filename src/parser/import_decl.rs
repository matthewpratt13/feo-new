use crate::{
    ast::{
        Identifier, ImportDecl, ImportTree, OuterAttr, PathPrefix, PathSegment, PathSubset,
        Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for ImportDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<ImportDecl, ErrorsEmitted> {
        let kw_import = parser.expect_keyword(Token::Import {
            name: "import".to_string(),
            span: parser.stream.span(),
        })?;

        let tree = ImportTree::parse(parser)?;

        parser.consume_token();

        if let Some(Token::Semicolon { .. }) = parser.peek_behind_by(1) {
            ()
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`;`".to_string(),
                found: parser.peek_current(),
            })
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if attributes.is_empty() {
            Ok(ImportDecl {
                attributes_opt: None,
                visibility,
                kw_import,
                tree,
            })
        } else {
            Ok(ImportDecl {
                attributes_opt: Some(attributes),
                visibility,
                kw_import,
                tree,
            })
        }
    }
}

impl ImportTree {
    fn parse(parser: &mut Parser) -> Result<ImportTree, ErrorsEmitted> {
        let mut segments: Vec<PathSegment> = Vec::new();

        let first_segment = PathSegment::parse(parser)?;
        segments.push(first_segment);

        while let Some(Token::DblColon { .. }) = parser.peek_current() {
            parser.consume_token();

            let next_segment = PathSegment::parse(parser)?;
            segments.push(next_segment);
        }

        Ok(ImportTree { segments })
    }
}

impl PathSegment {
    fn parse(parser: &mut Parser) -> Result<PathSegment, ErrorsEmitted> {
        let token = parser.peek_current();

        let root = match token {
            Some(Token::Package { .. }) => Ok(PathPrefix::Package),
            Some(Token::Super { .. }) => Ok(PathPrefix::Super),
            Some(Token::SelfKeyword { .. }) => Ok(PathPrefix::SelfKeyword),
            Some(Token::SelfType { .. }) => Ok(PathPrefix::SelfType),
            Some(Token::Identifier { name, .. }) => Ok(PathPrefix::Identifier(Identifier(name))),
            _ => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "path prefix".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }?;

        parser.consume_token();

        let subset_opt = if let Some(Token::LBrace { .. }) = parser.peek_ahead_by(1) {
            parser.consume_token();
            Some(PathSubset::parse(parser)?)
        } else {
            None
        };

        println!("CURRENT TOKEN: {:?}", parser.peek_current());

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(PathSegment { root, subset_opt })
    }
}

impl PathSubset {
    fn parse(parser: &mut Parser) -> Result<PathSubset, ErrorsEmitted> {
        let mut trees: Vec<ImportTree> = Vec::new();

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let tree = ImportTree::parse(parser)?;
            trees.push(tree);

            let token = parser.peek_current();

            match token {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `}`".to_string(),
                    found: Some(t),
                }),
                None => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: '}' });
                }
            }
        }

        // while let Some(Token::Identifier { .. }) = parser.consume_token() {
        //     let tree = ImportTree::parse(parser)?;
        //     trees.push(tree);

        //     match parser.peek_current() {
        //         Some(Token::Comma { .. }) => continue,
        //         Some(Token::RBrace { .. }) => break,
        //         Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
        //             expected: "`,` or `}`".to_string(),
        //             found: Some(t),
        //         }),
        //         None => {
        //             parser.log_error(ParserErrorKind::MissingDelimiter { delim: '}' });
        //         }
        //     }
        // }

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

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
        let input = r#"pub import package::module::Object;"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_import_decl_nested() -> Result<(), ()> {
        let input = r#"
        pub import package::module::{
            SomeObject,
            inner_module::{ 
                SomeInnerObject,
                AnotherInnerObject
            },
            another_inner_module::some_function,
            SOME_CONSTANT,
        };
        "#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
