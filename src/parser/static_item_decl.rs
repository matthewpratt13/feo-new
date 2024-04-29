use crate::{
    ast::{Identifier, Keyword, OuterAttr, StaticItemDecl, Type, Visibility},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{item::ParseDeclaration, Parser, Precedence};

impl ParseDeclaration for StaticItemDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<StaticItemDecl, ErrorsEmitted> {
        let kw_static = parser.expect_keyword(TokenType::Static)?;

        let kw_mut_opt = if let Some(Token::Mut { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Keyword::Mut)
        } else {
            None
        };

        let item_name = if let Some(Token::Identifier { name, .. }) = parser.consume_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        parser.expect_separator(TokenType::Colon)?;

        let item_type = Type::parse(parser)?;

        let value_opt = if let Some(Token::Equals { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Box::new(parser.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };

        parser.consume_token();

        parser.expect_separator(TokenType::Semicolon)?;

        Ok(StaticItemDecl {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_static,
            kw_mut_opt,
            item_name,
            item_type,
            value_opt,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_static_item_decl() -> Result<(), ()> {
        let input = r#"
        #[storage]
        pub(package) static mut foo: str = "bar";"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
