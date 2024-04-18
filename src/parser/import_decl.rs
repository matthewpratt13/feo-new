use crate::{
    ast::{Identifier, ImportDecl, OuterAttr, PathExpr, PathPrefix, PathSubset},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for ImportDecl {
    fn parse(parser: &mut Parser, attributes: Vec<OuterAttr>) -> Result<ImportDecl, ErrorsEmitted> {
        todo!()
    }
}

impl PathSubset {
    pub(crate) fn parse(parser: &mut Parser) -> Result<PathSubset, ErrorsEmitted> {
        let dbl_colon = parser.expect_separator(Token::DblColon {
            punc: "::".to_string(),
            span: parser.stream.span(),
        })?;

        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut trees: Vec<PathExpr> = Vec::new();

        while let Some(Token::Identifier { name, .. }) = parser.consume_token() {
            let tree = PathExpr::parse(parser, PathPrefix::Identifier(Identifier(name)))?;
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

        Ok(PathSubset {
            dbl_colon,
            open_brace,
            trees,
            close_brace,
        })
    }
}
