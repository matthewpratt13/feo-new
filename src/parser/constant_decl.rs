use crate::{
    ast::{ConstantDecl, Identifier, OuterAttr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{item::ParseDeclaration, Parser, Precedence};

impl ParseDeclaration for ConstantDecl {
    fn parse(parser: &mut Parser) -> Result<ConstantDecl, ErrorsEmitted> {
        let mut attributes: Vec<OuterAttr> = Vec::new();

        while let Some(oa) = parser.get_outer_attr() {
            parser.consume_token();
            attributes.push(oa);
        }

        let visibility = parser.get_visibility()?;

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

        let item_type = parser.get_type()?;

        let assignment_opt = if let Some(Token::Equals { .. }) = parser.consume_token() {
            Some(Box::new(parser.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };

        let semicolon = parser.expect_separator(Token::Semicolon {
            punc: ';',
            span: parser.stream.span(),
        })?;

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
                assignment_opt,
                semicolon,
            })
        } else {
            Ok(ConstantDecl {
                attributes_opt: Some(attributes),
                visibility,
                kw_const,
                item_name,
                item_type,
                assignment_opt,
                semicolon,
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
        const foo: str = "bar;""#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
