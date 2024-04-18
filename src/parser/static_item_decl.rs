use crate::{
    ast::{Identifier, Keyword, OuterAttr, StaticItemDecl},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{item::ParseDeclaration, Parser, Precedence};

impl ParseDeclaration for StaticItemDecl {
    fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
    ) -> Result<StaticItemDecl, ErrorsEmitted> {
        let visibility = parser.get_visibility()?;

        let kw_static = parser.expect_keyword(Token::Static {
            name: "static".to_string(),
            span: parser.stream.span(),
        })?;

        let kw_mut_opt = if let Some(Token::Mut { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Keyword::Mut)
        } else {
            None
        };

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

        let item_type = parser.get_type()?;

        let value_opt = if let Some(Token::Equals { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Box::new(parser.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        if attributes.is_empty() {
            Ok(StaticItemDecl {
                attributes_opt: None,
                visibility,
                kw_static,
                kw_mut_opt,
                item_name,
                item_type,
                value_opt,
            })
        } else {
            Ok(StaticItemDecl {
                attributes_opt: Some(attributes),
                visibility,
                kw_static,
                kw_mut_opt,
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
    fn parse_static_item_decl() -> Result<(), ()> {
        let input = r#"
        #[storage]
        static mut foo: str = "bar";"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
