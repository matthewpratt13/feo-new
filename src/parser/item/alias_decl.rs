use super::{ParseDeclItem, Parser};

use crate::{
    ast::{AliasDecl, Identifier, Keyword, OuterAttr, Type, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use core::fmt;

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
            parser.log_unexpected_token("`alias`");
            Err(ErrorsEmitted)
        }?;

        let alias_name = match parser.next_token() {
            Some(Token::Identifier { name, .. }) => Ok(Identifier::from(&name)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("identifier");
                Err(ErrorsEmitted)
            }
        }?;

        let original_type_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();

            if parser.current_token().is_some() {
                Ok(Some(Type::parse(parser)?))
            } else {
                parser.log_missing_token("original type");
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        match parser.current_token() {
            Some(Token::Semicolon { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());
                parser.next_token();

                Ok(AliasDecl {
                    attributes_opt,
                    visibility,
                    kw_alias,
                    alias_name,
                    original_type_opt,
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
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
