use crate::{
    ast::{Expression, Separator, TupleExpr, TupleIndexExpr},
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl TupleExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<TupleExpr, ErrorsEmitted> {
        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let mut elements: Vec<Expression> = Vec::new();

        loop {
            if let Some(Token::RParen { .. }) = parser.peek_current() {
                break;
            }

            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RParen { .. }) => break,

                _ => {
                    parser.log_error(ParserErrorKind::MissingDelimiter { delim: ')' });
                }
            }
        }

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        Ok(TupleExpr {
            open_paren,
            elements,
            close_paren,
        })
    }
}

impl TupleIndexExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        operand: Expression,
    ) -> Result<TupleIndexExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        let index = if let Some(Token::UIntLiteral { value, .. }) = token {
            Ok(value)
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "unsigned integer".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        };

        Ok(TupleIndexExpr {
            operand: Box::new(operand),
            dot: Separator::Dot,
            index: index?,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_tuple_expr() -> Result<(), ()> {
        let input = r#"(true, "foo", 10, x,)"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_tuple_index_expr() -> Result<(), ()> {
        let input = r#"tuple.0"#;

        let mut parser = test_utils::get_parser(input, true);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
