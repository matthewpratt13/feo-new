use crate::{
    ast::{
        Delimiter, Expression, Identifier, OuterAttr, PathExpr, StructExpr, StructField,
        TupleStructExpr,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{test_utils::log_token, Parser, Precedence};

impl StructExpr {
    pub(crate) fn parse(parser: &mut Parser, path: PathExpr) -> Result<Expression, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            log_token(parser, "consume token", true);
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let mut fields: Vec<StructField> = Vec::new();

        while !matches!(
            parser.peek_current(),
            Some(Token::RBrace { .. } | Token::EOF)
        ) {
            let mut attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                attributes.push(oa);
                parser.consume_token();
            }

            let field_name = if let Some(Token::Identifier { name, .. }) = parser.peek_current() {
                parser.consume_token();
                Ok(Identifier(name))
            } else {
                parser.expect_delimiter(TokenType::RBrace)?;
                Err(ErrorsEmitted)
            }?;

            parser.expect_separator(TokenType::Colon)?;

            let field_value = parser.parse_expression(Precedence::Lowest)?;

            let field = StructField {
                attributes_opt: {
                    if attributes.is_empty() {
                        None
                    } else {
                        Some(attributes)
                    }
                },
                field_name,
                field_value,
            };

            fields.push(field);

            if let Some(Token::Comma { .. }) = parser.peek_current() {
                parser.consume_token();
            } else if !matches!(
                parser.peek_current(),
                Some(Token::RBrace { .. } | Token::EOF)
            ) {
                parser.log_unexpected_str("`,` or `}`");
                return Err(ErrorsEmitted);
            }
        }

        let close_brace = if let Some(Token::RBrace { .. }) = parser.peek_current() {
            parser.consume_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = StructExpr {
            path,
            open_brace,
            fields_opt: {
                if fields.is_empty() {
                    None
                } else {
                    Some(fields)
                }
            },
            close_brace,
        };

        Ok(Expression::Struct(expr))
    }
}

// TODO: test when issue regarding similarity between `TupleStructExpr` and `CallExpr` is resolved
#[allow(dead_code)]
impl TupleStructExpr {
    pub(crate) fn parse(parser: &mut Parser, path: PathExpr) -> Result<Expression, ErrorsEmitted> {
        let open_paren = if let Some(Token::LParen { .. }) = parser.consume_token() {
            Ok(Delimiter::LParen)
        } else {
            parser.log_unexpected_token(TokenType::LParen);
            Err(ErrorsEmitted)
        }?;

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
                _ => break,
            }
        }

        let close_paren = parser.expect_delimiter(TokenType::RParen)?;

        let expr = TupleStructExpr {
            path,
            open_paren,
            elements_opt: {
                if elements.is_empty() {
                    None
                } else {
                    Some(elements)
                }
            },
            close_paren,
        };

        Ok(Expression::TupleStruct(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_struct_expr() -> Result<(), ()> {
        let input = r#"
        SomeStruct {
            foo: "bar",
            baz: -10,
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
