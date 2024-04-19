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

        if let Some(Token::Semicolon { .. }) = parser.consume_token() {
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

        while let Some(Token::DblColon { .. }) = parser.consume_token() {
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

        let subset = PathSubset::parse(parser)?;

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(PathSegment { root, subset })
    }
}

impl PathSubset {
    fn parse(parser: &mut Parser) -> Result<PathSubset, ErrorsEmitted> {
        let dbl_colon = parser.expect_separator(Token::DblColon {
            punc: "::".to_string(),
            span: parser.stream.span(),
        })?;

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut trees: Vec<ImportTree> = Vec::new();

        while let Some(Token::Identifier { .. }) = parser.consume_token() {
            let tree = ImportTree::parse(parser)?;
            trees.push(tree);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => continue,
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

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(PathSubset {
            dbl_colon,
            open_brace,
            trees,
            close_brace,
        })
    }
}
