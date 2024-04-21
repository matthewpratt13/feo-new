use crate::{ast::ReturnExpr, error::ErrorsEmitted, token::Token};

use super::{Parser, Precedence};

impl ReturnExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ReturnExpr, ErrorsEmitted> {
        let kw_return = parser.expect_keyword(Token::Return {
            name: "return".to_string(),
            span: parser.stream.span(),
        })?;

        let expression_opt: Option<Box<crate::ast::Expression>> =
            if let Some(t) = parser.peek_current() {
                Some(Box::new(parser.parse_expression(Precedence::Lowest)?))
            } else {
                None
            };

        Ok(ReturnExpr {
            kw_return,
            expression_opt,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_return_expr() -> Result<(), ()> {
        let input = r#"return Object { foo: "bar", baz: x };"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
