use crate::{
    ast::{AliasDecl, Identifier, Keyword, OuterAttr, Type, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use super::{ParseDeclaration, Parser};

impl ParseDeclaration for AliasDecl {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<AliasDecl, ErrorsEmitted> {
        let kw_alias = if let Some(Token::Alias { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Alias)
        } else {
            parser.log_unexpected_token("`alias`");
            Err(ErrorsEmitted)
        }?;

        let alias_name = if let Some(Token::Identifier { name, .. }) = parser.next_token() {
            Ok(Identifier(name))
            // TODO: handle `None` case (`UnexpectedEndOfInput`)
        } else {
            parser.log_unexpected_token("type alias name");
            Err(ErrorsEmitted)
        }?;

        let original_type_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();
            Some(Type::parse(parser)?)
        } else {
            None
        };

        if let Some(Token::Semicolon { .. }) = parser.current_token() {
            parser.next_token();
            Ok(AliasDecl {
                attributes_opt,
                visibility,
                kw_alias,
                alias_name,
                original_type_opt,
            })
        } else if let Some(_) = parser.current_token() {
            parser.log_unexpected_token("`;`");
            Err(ErrorsEmitted)
        } else {
            parser.log_missing_token("`;`");
            Err(ErrorsEmitted)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_alias_decl() -> Result<(), ()> {
        let input = r#"pub(package) alias Foo = (u64, bool, char);"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
