use crate::{
    ast::{Expression, OuterAttr, PathExpr, StructExpr, StructField, TupleStructExpr},
    error::ErrorsEmitted,
    parser::{
        collection, ParseConstructExpr, ParseOperatorExpr, ParseSimpleExpr, Parser, Precedence,
    },
    span::Spanned,
    token::{Token, TokenType},
};

use core::fmt;

impl ParseConstructExpr for StructExpr {
    fn parse(parser: &mut Parser) -> Result<StructExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let struct_path = PathExpr::parse(parser)?;

        let open_brace = parser.expect_delimiter(TokenType::LBrace)?;

        let struct_fields_opt =
            collection::get_collection(parser, parse_struct_field, &open_brace)?;

        let span = parser.get_braced_item_span(first_token.as_ref(), &open_brace)?;

        Ok(StructExpr {
            struct_path,
            struct_fields_opt,
            span,
        })
    }
}

impl fmt::Debug for StructExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructExpr")
            .field("struct_path", &self.struct_path)
            .field("struct_fields_opt", &self.struct_fields_opt)
            .finish()
    }
}

impl ParseOperatorExpr for TupleStructExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let open_paren = parser.expect_delimiter(TokenType::LParen)?;

        let struct_elements_opt =
            collection::get_expressions(parser, Precedence::Lowest, &open_paren)?;

        let last_token = parser.current_token();

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                let span = parser.get_span(left_expr_span, &last_token.unwrap().span());
                parser.next_token();

                Ok(Expression::TupleStruct(TupleStructExpr {
                    struct_path: PathExpr::from(left_expr),
                    struct_elements_opt,
                    span,
                }))
            }
            _ => {
                parser.log_unmatched_delimiter(&open_paren);
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for TupleStructExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TupleStructExpr")
            .field("struct_path", &self.struct_path)
            .field("struct_elements_opt", &self.struct_elements_opt)
            .finish()
    }
}

fn parse_struct_field(parser: &mut Parser) -> Result<StructField, ErrorsEmitted> {
    let attributes_opt = collection::get_attributes(parser, OuterAttr::outer_attr);

    let field_name = parser.expect_identifier()?;

    match parser.current_token() {
        Some(Token::Colon { .. }) => {
            parser.next_token();

            if let Some(Token::Comma { .. } | Token::RBrace { .. }) = parser.current_token() {
                parser.log_missing("expr", "value for struct field name");
                return Err(ErrorsEmitted);
            }
        }
        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            return Err(ErrorsEmitted);
        }
        _ => {
            parser.log_unexpected_token("`:`");
            return Err(ErrorsEmitted);
        }
    }

    let field_value = Box::new(parser.parse_expression(Precedence::Lowest)?);

    let struct_field = StructField {
        attributes_opt,
        field_name,
        field_value,
    };

    Ok(struct_field)
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_struct_expr() -> Result<(), ()> {
        let input = r#"
        SomeStruct {
            foo: "bar",
            baz: -10
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_tuple_struct_expr() -> Result<(), ()> {
        let input = r#"SomeStruct("bar", false, -1)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
