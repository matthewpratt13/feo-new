use super::ParseDeclItem;

use crate::{
    ast::{ConstantDecl, Identifier, Keyword, OuterAttr, Type, ValueExpr, Visibility},
    error::ErrorsEmitted,
    parser::{Parser, Precedence},
    token::{Token, TokenType},
};

use core::fmt;

impl ParseDeclItem for ConstantDecl {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<ConstantDecl, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_const = if let Some(Token::Const { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Const)
        } else {
            parser.log_unexpected_token("`const`");
            Err(ErrorsEmitted)
        }?;

        let constant_name = match parser.next_token() {
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

        parser.expect_token(TokenType::Colon)?;

        let constant_type = Box::new(Type::parse(parser)?);

        let value_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();

            if parser.current_token().is_some() {
                let expression = parser.parse_expression(Precedence::Lowest)?;
                let value_expr = ValueExpr::try_from(expression).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?;

                Ok(Some(value_expr))
            } else {
                parser.log_missing("expr", "value");
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        match parser.current_token() {
            Some(Token::Semicolon { .. }) => {
                let span = parser.get_span_by_token(&first_token.unwrap());

                parser.next_token();

                Ok(ConstantDecl {
                    attributes_opt,
                    visibility,
                    kw_const,
                    constant_name,
                    constant_type,
                    value_opt,
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

impl fmt::Debug for ConstantDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConstantDecl")
            .field("attributes_opt", &self.attributes_opt)
            .field("visibility", &self.visibility)
            .field("constant_name", &self.constant_name)
            .field("constant_type", &self.constant_type)
            .field("value_opt", &self.value_opt)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_constant_decl() -> Result<(), ()> {
        let input = r#"
        #[storage]
        pub const foo: str = "bar";"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
