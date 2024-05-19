use crate::{
    ast::{
        Delimiter, Identifier, ImportDecl, ImportTree, Keyword, OuterAttr, PathSegment, PathSubset,
        PathType, Separator, Visibility,
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
        let kw_import = if let Some(Token::Import { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Import)
        } else {
            parser.log_unexpected_token("`import`");
            Err(ErrorsEmitted)
        }?;

        let import_tree = parse_import_tree(parser)?;

        match parser.current_token() {
            Some(Token::Semicolon { .. }) => {
                parser.next_token();
                Ok(ImportDecl {
                    attributes_opt,
                    visibility,
                    kw_import,
                    import_tree,
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
        Some(Separator::ColonColonAsterisk)
    } else {
        None
    };

    let as_clause_opt = if let Some(Token::As { .. }) = parser.current_token() {
        let kw_as = Keyword::As;
        parser.next_token();

        let id = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("identifier");
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

fn parse_path_segment(parser: &mut Parser) -> Result<PathSegment, ErrorsEmitted> {
    let root = PathType::parse(parser, parser.current_token())?;

    let subset_opt = if let Some(Token::LBrace { .. }) = parser.peek_ahead_by(1) {
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
        let input = r#"pub import package::some_module::SomeObject;"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let item = parser.parse_item();

        match item {
            Ok(i) => Ok(println!("{:#?}", i)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
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

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let item = parser.parse_item();

        match item {
            Ok(i) => Ok(println!("{:#?}", i)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_import_decl_with_as_clause() -> Result<(), ()> {
        let input = r#"pub import package::some_module::SomeObject as AnotherObject;"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let item = parser.parse_item();

        match item {
            Ok(i) => Ok(println!("{:#?}", i)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
