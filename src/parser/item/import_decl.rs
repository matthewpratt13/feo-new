use core::fmt;

use crate::{
    ast::{
        ImportDecl, ImportTree, Keyword, OuterAttr, PathSegment, PathSubset, PathWildcard,
        TypePath, Visibility,
    },
    error::ErrorsEmitted,
    parser::{get_collection, ParseDeclItem, Parser},
    token::{Token, TokenType},
};

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
            parser.emit_unexpected_token(&TokenType::Import.to_string());
            Err(ErrorsEmitted)
        }?;

        let import_tree = parse_import_tree(parser)?;

        let span = parser.get_decl_item_span(first_token.as_ref())?;

        Ok(ImportDecl {
            attributes_opt,
            visibility,
            kw_import,
            import_tree,
            span,
        })
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
        Some(parser.expect_identifier("new type name")?)
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
    let open_brace = parser.expect_open_brace()?;

    let nested_trees = if let Some(t) = get_collection(parser, parse_import_tree, &open_brace)? {
        Ok(t)
    } else {
        parser.emit_missing_node("path component", "import declaration path import tree");
        parser.next_token();
        Err(ErrorsEmitted)
    }?;

    let _ = parser.get_braced_item_span(None)?;

    Ok(PathSubset { nested_trees })
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_import_decl_nested() -> Result<(), ()> {
        let input = r#"
        pub import lib::some_module::{
            SomeObject,
            AnotherObject::another_method,
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_import_decl_with_as_clause() -> Result<(), ()> {
        let input = r#"pub import lib::some_module::SomeObject as AnotherObject;"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
