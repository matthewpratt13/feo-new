use crate::{
    ast::{ConstantDecl, Identifier, OuterAttr, Type, ValueExpr, Visibility},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{item::ParseDeclaration, Parser, Precedence};

impl ParseDeclaration for ConstantDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<ConstantDecl, ErrorsEmitted> {
        let kw_const = parser.expect_keyword(TokenType::Const)?;

        let item_name = if let Some(Token::Identifier { name, .. }) = parser.consume_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        parser.expect_separator(TokenType::Colon)?;

        let item_type = Box::new(Type::parse(parser)?);

        let value = if let Some(Token::Equals { .. }) = parser.peek_current() {
            parser.consume_token();
            let expr = parser.parse_expression(Precedence::Lowest)?;
            Ok(ValueExpr::try_from(expr).map_err(|e| {
                parser.log_error(e);
                ErrorsEmitted
            })?)
        } else {
            parser.log_unexpected_str("value expression");
            Err(ErrorsEmitted)
        }?;


        parser.expect_separator(TokenType::Semicolon)?;

        Ok(ConstantDecl {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },

            visibility,
            kw_const,
            item_name,
            item_type,
            value,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_constant_decl() -> Result<(), ()> {
        let input = r#"
        #[storage]
        pub const foo: str = "bar";"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
