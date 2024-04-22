use crate::{
    ast::{AliasDecl, Identifier, OuterAttr, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for AliasDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<AliasDecl, ErrorsEmitted> {
        let visibility = parser.get_visibility()?;

        let kw_alias = parser.expect_keyword(Token::Alias {
            name: "alias".to_string(),
            span: parser.stream.span(),
        })?;

        let alias_name = if let Some(Token::Identifier { name, .. }) = parser.consume_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("identifier".to_string());
            Err(ErrorsEmitted(()))
        }?;

        let original_type_opt = if let Some(Token::Equals { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(parser.get_type()?)
        } else {
            None
        };

        let semicolon = parser.expect_separator(Token::Semicolon {
            punc: ';',
            span: parser.stream.span(),
        })?;

        Ok(AliasDecl {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_alias,
            alias_name,
            original_type_opt,
            semicolon,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_alias_decl() -> Result<(), ()> {
        let input = r#"pub(package) alias Foo = (u64, bool, char);"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
