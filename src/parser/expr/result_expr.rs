use core::fmt;

use crate::{
    ast::{GroupedExpr, Keyword, ResultExpr},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, Parser},
    token::Token,
};

impl ParseConstructExpr for ResultExpr {
    fn parse(parser: &mut Parser) -> Result<ResultExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_ok_or_err = match &first_token {
            Some(Token::Ok { .. }) => Ok(Keyword::Ok),
            Some(Token::Err { .. }) => Ok(Keyword::Err),
            _ => {
                parser.log_unexpected_token("`Ok` or `Err`");
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let expression = match parser.current_token() {
            Some(Token::LParen { .. }) => Ok(GroupedExpr::parse(parser)?),
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let span = parser.get_span(&first_token.unwrap().span(), &expression.span);

        let expr = ResultExpr {
            kw_ok_or_err,
            expression: Box::new(expression),
            span,
        };

        Ok(expr)
    }
}

impl fmt::Debug for ResultExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResultExpr")
            .field("kw_ok_or_err", &self.kw_ok_or_err)
            .field("expression", &self.expression)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_result_expr() -> Result<(), ()> {
        let input = r#"Ok(())"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
