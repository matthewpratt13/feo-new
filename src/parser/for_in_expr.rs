use crate::{
    ast::{BlockExpr, ForInExpr, Pattern},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl ForInExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ForInExpr, ErrorsEmitted> {
        let kw_for = parser.expect_keyword(Token::For {
            name: "for".to_string(),
            span: parser.stream.span(),
        })?;

        let assignee =
            if let Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) =
                parser.peek_current()
            {
                parser.get_identifier_patt()
            } else {
                let expression = parser.parse_expression(Precedence::Lowest)?;
                Pattern::try_from(expression).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })
            }?;
        let kw_in = parser.expect_keyword(Token::In {
            name: "in".to_string(),
            span: parser.stream.span(),
        })?;

        let iterable = parser.parse_expression(Precedence::Lowest)?;

        let block = BlockExpr::parse(parser)?;

        Ok(ForInExpr {
            kw_for,
            assignee,
            kw_in,
            iterable: Box::new(iterable),
            block,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_for_in_expr() -> Result<(), ()> {
        let input = r#"
        for x in 0..=5 {
            x += 1;

            let y = 15;

            for z in y {
                print("foo");
            }
        }
        "#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
