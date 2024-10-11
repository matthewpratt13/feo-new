use core::fmt;

use crate::{
    ast::{AliasDecl, Keyword, OuterAttr, Type, Visibility},
    error::ErrorsEmitted,
    parser::{ParseDeclItem, Parser},
    token::{Token, TokenType},
};

impl ParseDeclItem for AliasDecl {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<AliasDecl, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_alias = if let Some(Token::Alias { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Alias)
        } else {
            parser.emit_unexpected_token(&TokenType::Alias.to_string());
            Err(ErrorsEmitted)
        }?;

        let alias_name = parser.expect_identifier("alias name")?;

        let original_type_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();

            if parser.current_token().is_some() {
                Ok(Some(Type::parse(parser)?))
            } else {
                parser.emit_missing_node("type", "original type");
                parser.next_token();
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        let span = parser.get_decl_item_span(first_token.as_ref())?;

        Ok(AliasDecl {
            attributes_opt,
            visibility,
            kw_alias,
            alias_name,
            original_type_opt,
            span,
        })
    }
}

impl fmt::Debug for AliasDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AliasDecl")
            .field("attributes_opt", &self.attributes_opt)
            .field("visibility", &self.visibility)
            .field("alias_name", &self.alias_name)
            .field("original_type_opt", &self.original_type_opt)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_alias_decl() -> Result<(), ()> {
        let input = r#"pub(lib) alias Foo = (u64, bool, char);"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
