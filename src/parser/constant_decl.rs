use crate::{
    ast::{ConstantDecl, Identifier, Keyword, OuterAttr, Type, ValueExpr, Visibility},
    error::ErrorsEmitted,
    token::Token,
};

use super::{parse::ParseDeclaration, Parser, Precedence};

impl ParseDeclaration for ConstantDecl {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<ConstantDecl, ErrorsEmitted> {
        let kw_const = if let Some(Token::Const { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Const)
        } else {
            parser.log_unexpected_token("`const`");
            Err(ErrorsEmitted)
        }?;

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

        let item_type = Box::new(Type::parse(parser)?);

        let value_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();

            if parser.current_token().is_some() {
                let expr = parser.parse_expression(Precedence::Lowest)?;
                Ok(Some(ValueExpr::try_from(expr).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?))
            } else {
                parser.log_missing_token("value");
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        if let Some(Token::Semicolon { .. }) = parser.current_token() {
            parser.next_token();

            Ok(ConstantDecl {
                attributes_opt,
                visibility,
                kw_const,
                item_name,
                item_type,
                value_opt,
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
    fn parse_constant_decl() -> Result<(), ()> {
        let input = r#"
        #[storage]
        pub const foo: str = "bar";"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
