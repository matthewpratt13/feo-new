use crate::{
    ast::{AssignmentOp, ExpressionStmt, Keyword, LetStmt, PlaceExpr, Separator},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

pub(crate) trait ParseStatement
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

impl ParseStatement for LetStmt {
    fn parse(parser: &mut Parser) -> Result<LetStmt, ErrorsEmitted> {
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

        let assignee = PlaceExpr::try_from(parser.parse_expression(Precedence::Path)?)?;

        let type_ann_opt = if let Some(Token::Colon { .. }) = parser.peek_current() {
            parser.consume_token();
            Some((Separator::Colon, parser.get_type()?))
        } else {
            None
        };

        let value_opt = if let Some(Token::Equals { .. }) = parser.consume_token() {
            let value = parser.parse_expression(Precedence::Lowest)?;
            Some((AssignmentOp::Assign, value))
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

impl ParseStatement for ExpressionStmt {
    fn parse(parser: &mut Parser) -> Result<ExpressionStmt, ErrorsEmitted> {
        println!("ENTER `ExpressionStmt::parse()`\n");

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let semicolon_opt = if let Some(Token::Semicolon { .. }) = parser.peek_current() {
            println!("ENCOUNTER `;`");

            parser.consume_token();

            println!("SKIP `;`");
            println!("CURRENT TOKEN: {:?}", parser.peek_current());

            Some(Separator::Semicolon)
        } else {
            None
        };

        println!("EXIT `ExpressionStmt::parse()`");
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        Ok(ExpressionStmt {
            expression,
            semicolon_opt,
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

    #[test]
    fn parse_expression_stmt() -> Result<(), ()> {
        let input = r#"x + 2;"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
