use core::fmt;

use crate::{
    ast::{
        Delimiter, Identifier, ImportDecl, ImportTree, Keyword, OuterAttr, PathSegment, PathSubset,
        PathWildcard, TypePath, Visibility,
    },
    error::ErrorsEmitted,
    span::Position,
    token::Token,
};

use super::{collection, ParseDeclItem, Parser};

impl ParseDeclItem for ImportDecl {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<ImportDecl, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_import = if let Some(Token::Import { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Import)
        } else {
            parser.log_unexpected_token("`import`");
            Err(ErrorsEmitted)
        }?;

        let import_tree = parse_import_tree(parser)?;

        match parser.current_token() {
            Some(Token::Semicolon { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());

                parser.next_token();

                Ok(ImportDecl {
                    attributes_opt,
                    visibility,
                    kw_import,
                    import_tree,
                    span,
                })
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`;`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`;`");
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for ImportDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ImportDecl")
            .field("attributes_opt", &self.attributes_opt)
            .field("visibility", &self.visibility)
            .field("import_tree", &self.import_tree)
            .finish()
    }
}

fn parse_import_tree(parser: &mut Parser) -> Result<ImportTree, ErrorsEmitted> {
    let mut path_segments: Vec<PathSegment> = Vec::new();

    let first_segment = parse_path_segment(parser)?;
    path_segments.push(first_segment);

    while let Some(Token::DblColon { .. }) = parser.current_token() {
        parser.next_token();
        let next_segment = parse_path_segment(parser)?;
        path_segments.push(next_segment);
    }

    let wildcard_opt = if let Some(Token::ColonColonAsterisk { .. }) = parser.current_token() {
        parser.next_token();
        Some(PathWildcard)
    } else {
        None
    };

    let as_clause_opt = if let Some(Token::As { .. }) = parser.current_token() {
        parser.next_token();

        let id = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier::from(&name))
        } else {
            parser.log_unexpected_token("identifier");
            Err(ErrorsEmitted)
        }?;

        Some(id)
    } else {
        None
    };

    Ok(ImportTree {
        path_segments,
        wildcard_opt,
        as_clause_opt,
    })
}

fn parse_path_segment(parser: &mut Parser) -> Result<PathSegment, ErrorsEmitted> {
    let root = TypePath::parse(parser, parser.current_token().cloned())?;

    let subset_opt = if let Some(Token::LBrace { .. }) = &parser.peek_ahead_by(1) {
        parser.next_token();
        Some(parse_path_subset(parser)?)
    } else {
        None
    };

    Ok(PathSegment { root, subset_opt })
}

fn parse_path_subset(parser: &mut Parser) -> Result<PathSubset, ErrorsEmitted> {
    let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
        let position = Position::new(parser.current, &parser.stream.span().input());
        parser.next_token();
        Ok(Delimiter::LBrace { position })
    } else {
        parser.log_unexpected_token("`{`");
        Err(ErrorsEmitted)
    }?;

    let nested_trees =
        if let Some(t) = collection::get_collection(parser, parse_import_tree, &open_brace)? {
            Ok(t)
        } else {
            parser.log_missing("path component", "import declaration path import tree");
            Err(ErrorsEmitted)
        }?;

    match parser.current_token() {
        Some(Token::RBrace { .. }) => {
            parser.next_token();

            Ok(PathSubset { nested_trees })
        }
        Some(Token::EOF) | None => {
            parser.log_unmatched_delimiter(&open_brace);
            parser.log_unexpected_eoi();
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("`}`");
            Err(ErrorsEmitted)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_import_decl_simple() -> Result<(), ()> {
        let input = r#"pub import lib::some_module::SomeObject;"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_import_decl_nested() -> Result<(), ()> {
        let input = r#"
        pub import lib::some_module::{
            SomeObject,
            inner_module::{ 
                SomeInnerObject,
                AnotherInnerObject
            },
            another_inner_module::*,
            SOME_CONSTANT,
        };"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_import_decl_with_as_clause() -> Result<(), ()> {
        let input = r#"pub import lib::some_module::SomeObject as AnotherObject;"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
