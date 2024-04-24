use crate::{
    ast::{AssignmentOp, Keyword, LetStmt, Pattern, Separator},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl LetStmt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<LetStmt, ErrorsEmitted> {
        let kw_let = if let Some(Token::Let { .. }) = parser.consume_token() {
            Ok(Keyword::Let)
        } else {
            parser.log_unexpected_token("`let`".to_string());
            Err(ErrorsEmitted(()))
        }?;

        let kw_mut_opt = if let Some(Token::Mut { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Keyword::Mut)
        } else {
            None
        };

        let assignee =
            Pattern::try_from(parser.parse_expression(Precedence::Path)?).map_err(|e| {
                parser.log_error(e);
                ErrorsEmitted(())
            })?;

        let type_ann_opt = if let Some(Token::Colon { .. }) = parser.peek_current() {
            parser.consume_token();
            Some((Separator::Colon, parser.get_type()?))
        } else {
            None
        };

        let value_opt = if let Some(Token::Equals { .. }) = parser.consume_token() {
            let value = parser.parse_expression(Precedence::Lowest)?;
            Some((AssignmentOp(()), value))
        } else {
            None
        };

        let _ = parser.expect_separator(Token::Semicolon {
            punc: ';',
            span: parser.stream.span(),
        })?;

        Ok(LetStmt {
            kw_let,
            kw_mut_opt,
            assignee,
            type_ann_opt,
            value_opt,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_let_stmt() -> Result<(), ()> {
        let input = r#"let x: str = "hello world";"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
