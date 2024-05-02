use crate::{
    ast::{Delimiter, Expression, Keyword, ResultExpr},
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

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

        if let Some(Token::LParen { .. }) = parser.next_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        parser.expect_delimiter(TokenType::RParen)?;

        let expr = ResultExpr {
            kw_ok_or_err,
            expression: Box::new(expression),
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
