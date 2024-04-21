use crate::{
    ast::{Keyword, ResultExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl ResultExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ResultExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        let kw_ok_or_err = if let Some(Token::Ok { .. }) = token {
            Ok(Keyword::Ok)
        } else if let Some(Token::Err { .. }) = token {
            Ok(Keyword::Err)
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`Ok` or `Err`".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        }?;

        let _ = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let _ = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        Ok(ResultExpr {
            kw_ok_or_err,
            expression: Box::new(expression),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_result_expr() -> Result<(), ()> {
        let input = r#"Ok(x + 2)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
