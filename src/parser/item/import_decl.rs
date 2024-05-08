use crate::{
    ast::{
        Delimiter, Identifier, ImportDecl, ImportTree, Keyword, OuterAttr, PathExpr, PathPrefix,
        PathSegment, PathSubset, Separator, Visibility,
    },
    error::ErrorsEmitted,
    token::Token,
};

use super::{collection, ParseDeclaration, Parser};

impl ParseDeclaration for ImportDecl {
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

        let tree = parse_import_tree(parser)?;

        if let Some(Token::Semicolon { .. }) = parser.current_token() {
            parser.next_token();
            Ok(ImportDecl {
                attributes_opt,
                visibility,
                kw_import,
                tree,
            })
        } else if let Some(_) = parser.current_token() {
            parser.log_unexpected_token("`;`");
            Err(ErrorsEmitted)
        } else {
            parser.log_missing_token("`;`");
            Err(ErrorsEmitted)
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
    let root = match parser.next_token() {
        Some(Token::Package { .. }) => PathExpr::parse(parser, PathPrefix::Package),
        Some(Token::Super { .. }) => PathExpr::parse(parser, PathPrefix::Super),
        Some(Token::SelfKeyword { .. }) => PathExpr::parse(parser, PathPrefix::SelfKeyword),
        Some(Token::Identifier { .. }) => PathExpr::parse(parser, PathPrefix::Package),
        _ => {
            parser.log_unexpected_token("`package`, `super`, `self` or identifier path prefix");
            Err(ErrorsEmitted)
        }
    }?;

    let subset_opt = if let Some(Token::LBrace { .. }) = parser.peek_ahead_by(1) {
        parser.next_token();
        Some(parse_path_subset(parser)?)
    } else {
        None
    };

    Ok(PathSegment { root, subset_opt })
}

fn parse_path_subset(parser: &mut Parser) -> Result<PathSubset, ErrorsEmitted> {
    let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
        Ok(Delimiter::LBrace)
    } else {
        parser.log_unexpected_token("`{`");
        Err(ErrorsEmitted)
    }?;

    let trees = if let Some(t) =
        collection::get_collection(parser, parse_import_tree, Delimiter::RBrace)?
    {
        Ok(t)
    } else {
        // TODO: should be `UnexpectedItems`
        parser.log_unexpected_token("import trees");
        Err(ErrorsEmitted)
    }?;

    let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
        Ok(Delimiter::RBrace)
    } else {
        parser.log_missing_token("`}`");
        parser.log_unmatched_delimiter(&open_brace);
        Err(ErrorsEmitted)
    }?;

    Ok(PathSubset {
        open_brace,
        trees,
        close_brace,
    })
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_import_decl_simple() -> Result<(), ()> {
        let input = r#"pub import package::some_module::SomeObject;"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_import_decl_with_as_clause() -> Result<(), ()> {
        let input = r#"pub import package::some_module::SomeObject as AnotherObject;"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
