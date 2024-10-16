use core::fmt;

use crate::{
    ast::{ConstantDecl, Keyword, OuterAttr, Type, ValueExpr, Visibility},
    error::ErrorsEmitted,
    parser::{ParseDeclItem, Parser, Precedence},
    token::{Token, TokenType},
};

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
            parser.emit_unexpected_token(&TokenType::Const.to_string());
            Err(ErrorsEmitted)
        }?;

        let constant_name = parser.expect_identifier("constant name")?;

        parser.expect_token(TokenType::Colon)?;

        let constant_type = match parser.current_token() {
            Some(Token::Semicolon { .. }) => {
                parser.emit_missing_node("type", "constant type");
                Err(ErrorsEmitted)
            }
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.emit_missing_node("type", "constant type");
                Err(ErrorsEmitted)
            }
            _ => Type::parse(parser),
        }?;

        let value_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();

            if parser.current_token().is_some() {
                let expression = parser.parse_expression(Precedence::Lowest)?;
                let value_expr = ValueExpr::try_from(expression).map_err(|e| {
                    parser.emit_error(e);
                    ErrorsEmitted
                })?;

                Ok(Some(value_expr))
            } else {
                parser.emit_missing_node("expr", "constant value");
                parser.next_token();
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        let span = parser.get_decl_item_span(first_token.as_ref())?;

        Ok(ConstantDecl {
            attributes_opt,
            visibility,
            kw_const,
            constant_name,
            constant_type: Box::new(constant_type),
            value_opt,
            span,
        })
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
            Ok(stmt) => Ok(println!("{stmt:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
