use crate::{
    ast::{Identifier, Keyword, OuterAttr, StaticItemDecl, Type, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use super::{parse::ParseDeclaration, Parser, Precedence};

impl ParseDeclaration for StaticItemDecl {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<StaticItemDecl, ErrorsEmitted> {
        let kw_static = if let Some(Token::Static { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Static)
        } else {
            parser.log_unexpected_token("`static`");
            Err(ErrorsEmitted)
        }?;

        let kw_mut_opt = if let Some(Token::Mut { .. }) = parser.current_token() {
            parser.next_token();
            Some(Keyword::Mut)
        } else {
            None
        };

        let item_name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
        } else {
            parser.log_unexpected_token("identifier");
            Err(ErrorsEmitted)
        }?;

        match parser.next_token() {
            Some(Token::Colon { .. }) => (),
            Some(_) => parser.log_unexpected_token("`:`"),
            None => parser.log_missing_token("`:`"),
        }

        let item_type = Type::parse(parser)?;

        let value_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();
            Some(Box::new(parser.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };

        match parser.next_token() {
            Some(Token::Semicolon { .. }) => (),
            Some(_) => parser.log_unexpected_token("`;`"),
            None => parser.log_missing_token("`;`"),
        }

        Ok(StaticItemDecl {
            attributes_opt,
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
