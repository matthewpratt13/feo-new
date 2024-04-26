use crate::{
    ast::{
        Delimiter, Expression, Identifier, OuterAttr, PathExpr, StructExpr, StructField,
        TupleStructExpr,
    },
    error::ErrorsEmitted,
    token::{Token, TokenType},
};

use super::{Parser, Precedence};

impl StructExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        path: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.consume_token() {
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let mut fields: Vec<StructField> = Vec::new();

        loop {
            if let Some(Token::RBrace { .. }) = parser.peek_current() {
                break;
            }

            let mut attributes: Vec<OuterAttr> = Vec::new();

            while let Some(oa) = parser.get_outer_attr() {
                attributes.push(oa);
                parser.consume_token();
            }

            let name = match parser.consume_token() {
                Some(Token::Identifier { name, .. }) => Ok(name),
                Some(Token::RBrace { .. }) => break,
                _ => {
                    parser.expect_delimiter(TokenType::RBrace)?;
                    Err(ErrorsEmitted)
                }
            }?;

            let _ = parser.expect_separator(TokenType::Colon);

            let value = parser.parse_expression(Precedence::Lowest)?;

            let field = StructField {
                attributes_opt: {
                    if attributes.is_empty() {
                        None
                    } else {
                        Some(attributes)
                    }
                },
                field_name: Identifier(name),
                field_value: value,
            };

            fields.push(field);

            match parser.peek_current() {
                Some(Token::Comma { .. }) => {
                    parser.consume_token();
                    continue;
                }
                Some(Token::RBrace { .. }) => break,
                _ => break,
            }
        }

        let close_brace = parser.expect_delimiter(TokenType::RBrace)?;

        let expr = StructExpr {
            path: Box::new(path),
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

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
