use crate::{
    ast::{
        Delimiter, ImportDecl, ImportTree, OuterAttr, PathExpr, PathPrefix, PathSegment,
        PathSubset, Visibility,
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

        let _ = parser.expect_separator(Token::Semicolon {
            punc: ';',
            span: parser.stream.span(),
        })?;

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
        let token = parser.consume_token();

        let root = match token {
            Some(Token::Package { .. }) => PathExpr::parse(parser, PathPrefix::Package),
            Some(Token::Super { .. }) => PathExpr::parse(parser, PathPrefix::Super),
            Some(Token::SelfKeyword { .. }) => PathExpr::parse(parser, PathPrefix::SelfKeyword),
            Some(Token::Identifier { name, .. }) => PathExpr::parse(parser, PathPrefix::Package),
            _ => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "path prefix".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }?;

        let subset_opt = if let Some(Token::LBrace { .. }) = parser.peek_ahead_by(1) {
            parser.consume_token();
            Some(PathSubset::parse(parser)?)
        } else {
            None
        };

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(PathSegment { root, subset_opt })
    }
}

impl PathSubset {
    fn parse(parser: &mut Parser) -> Result<PathSubset, ErrorsEmitted> {
        let mut trees: Vec<ImportTree> = Vec::new();

        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`".to_string());
            Err(ErrorsEmitted(()))
        }?;
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
                Some(Token::ColonColonAsterisk { .. }) => break,
                Some(t) => parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `}`".to_string(),
                    found: Some(t),
                }),
                None => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: '}' });
                }
            }
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_delimiter('}');
            Err(ErrorsEmitted(()))
        }?;

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
            another_inner_module::*,
            SOME_CONSTANT,
        };"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
