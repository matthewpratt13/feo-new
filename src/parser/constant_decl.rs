use crate::{
    ast::{ConstantDecl, Identifier, OuterAttr, Visibility},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{item::ParseDeclaration, Parser, Precedence};

impl ParseDeclaration for ConstantDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<ConstantDecl, ErrorsEmitted> {
        let kw_const = parser.expect_keyword(Token::Const {
            name: "const".to_string(),
            span: parser.stream.span(),
        })?;

        let item_name = if let Some(Token::Identifier { name, .. }) = parser.consume_token() {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: parser.peek_current(),
            });
            Err(ErrorsEmitted(()))
        }?;

        let _ = parser.expect_separator(Token::Colon {
            punc: ':',
            span: parser.stream.span(),
        })?;

        let item_type = Box::new(parser.get_type()?);

        let value_opt = if let Some(Token::Equals { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Box::new(parser.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };

        if let Some(Token::Semicolon { .. }) = parser.peek_behind_by(1) {
            ()
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`;`".to_string(),
                found: parser.peek_current(),
            })
        }

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if attributes.is_empty() {
            Ok(ConstantDecl {
                attributes_opt: None,
                visibility,
                kw_const,
                item_name,
                item_type,
                value_opt,
            })
        } else {
            Ok(ConstantDecl {
                attributes_opt: Some(attributes),
                visibility,
                kw_const,
                item_name,
                item_type,
                value_opt,
            })
        }
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
