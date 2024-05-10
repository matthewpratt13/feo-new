use crate::{
    ast::{AssigneeExpr, Identifier, Keyword, OuterAttr, StaticItemDecl, Type, Visibility},
    error::ErrorsEmitted,
    parser::Precedence,
    token::Token,
};

use super::{ParseDeclaration, Parser};

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

        let item_name = match parser.next_token() {
            Some(Token::Identifier { name, .. }) => Ok(Identifier(name)),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("static item identifier");
                Err(ErrorsEmitted)
            }
        }?;

        match parser.current_token() {
            Some(Token::Colon { .. }) => {
                parser.next_token();
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`:`");
                return Err(ErrorsEmitted);
            }
            _ => {
                parser.log_unexpected_token("`:`");
                return Err(ErrorsEmitted);
            }
        }

        let item_type = Type::parse(parser)?;

        let assignee_opt = if let Some(Token::Equals { .. }) = parser.current_token() {
            parser.next_token();

            if parser.current_token().is_some() {
                let expression = parser.parse_expression(Precedence::Lowest)?;
                let assignee_expr = AssigneeExpr::try_from(expression).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?;

                Ok(Some(Box::new(assignee_expr)))
            } else {
                parser.log_missing("expr", "assignee");
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        match parser.current_token() {
            Some(Token::Semicolon { .. }) => {
                parser.next_token();

                Ok(StaticItemDecl {
                    attributes_opt,
                    visibility,
                    kw_static,
                    kw_mut_opt,
                    item_name,
                    item_type,
                    assignee_opt,
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

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_static_item_decl() -> Result<(), ()> {
        let input = r#"
        #[storage]
        pub(package) static mut foo: str = "bar";"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
