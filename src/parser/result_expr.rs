use crate::{
    ast::{Expression, GroupedExpr, Keyword, ResultExpr},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::Parser;

impl ResultExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let token = parser.next_token();

        let kw_ok_or_err = if let Some(Token::Ok { .. }) = token {
            Ok(Keyword::Ok)
        } else if let Some(Token::Err { .. }) = token {
            Ok(Keyword::Err)
        } else {
            parser.log_unexpected_str("`Ok` or `Err`");
            Err(ErrorsEmitted)
        }?;

        let expression = if let Some(Token::LParen { .. }) = parser.current_token() {
            Ok(Box::new(GroupedExpr::parse(parser)?))
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let expr = ResultExpr {
            kw_ok_or_err,
            expression,
        };

        Ok(Expression::ResultExpr(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_result_expr() -> Result<(), ()> {
        let input = r#"Ok(foo)"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
