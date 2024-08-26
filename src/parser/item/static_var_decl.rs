use super::{ParseDeclItem, Parser};

use crate::{
    ast::{AssigneeExpr, Keyword, OuterAttr, StaticVarDecl, Type, Visibility},
    error::ErrorsEmitted,
    parser::Precedence,
    token::{Token, TokenType},
};

use core::fmt;

impl ParseDeclItem for StaticVarDecl {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<StaticVarDecl, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_static = if let Some(Token::Static { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::Static)
        } else {
            parser.log_unexpected_token(&TokenType::Static.to_string());
            Err(ErrorsEmitted)
        }?;

        let kw_mut_opt = if let Some(Token::Mut { .. }) = parser.current_token() {
            parser.next_token();
            Some(Keyword::Mut)
        } else {
            None
        };

        let var_name = parser.expect_identifier("variable name")?;

        parser.expect_token(TokenType::Colon)?;

        let var_type = Type::parse(parser)?;

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
                parser.next_token();
                Err(ErrorsEmitted)
            }
        } else {
            Ok(None)
        }?;

        let span = parser.get_decl_item_span(first_token.as_ref())?;

        Ok(StaticVarDecl {
            attributes_opt,
            visibility,
            kw_static,
            kw_mut_opt,
            var_name,
            var_type,
            assignee_opt,
            span,
        })
    }
}

impl fmt::Debug for StaticVarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StaticVarDecl")
            .field("attributes_opt", &self.attributes_opt)
            .field("visibility", &self.visibility)
            .field("kw_mut_opt", &self.kw_mut_opt)
            .field("var_name", &self.var_name)
            .field("var_type", &self.var_type)
            .field("assignee_opt", &self.assignee_opt)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_static_item_decl() -> Result<(), ()> {
        let input = r#"
        #[storage]
        pub(lib) static mut foo: str = "bar";"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statement = parser.parse_statement();

        match statement {
            Ok(s) => Ok(println!("{:#?}", s)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
