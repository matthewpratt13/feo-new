use crate::{
    ast::{AliasDecl, Identifier, OuterAttr, Type, Visibility},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{item::ParseDeclaration, Parser};

impl ParseDeclaration for AliasDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        _visibility: Visibility,
    ) -> Result<AliasDecl, ErrorsEmitted> {
        let visibility = parser.get_visibility()?;

        let kw_alias = parser.expect_keyword(TokenType::Alias)?;

        let alias_name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        let original_type_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();
            Some(Type::parse(parser)?)
        } else {
            None
        };

        parser.expect_separator(TokenType::Semicolon)?;

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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
