use core::fmt;

use crate::{
    ast::{Expression, OuterAttr, PathExpr, StructExpr, StructField, TupleStructExpr},
    error::ErrorsEmitted,
    parser::{
        get_attributes, get_collection, get_expressions, ParseConstructExpr, ParseOperatorExpr,
        ParseSimpleExpr, Parser, Precedence,
    },
    span::Spanned,
    token::{Token, TokenType},
};

impl ParseConstructExpr for StructExpr {
    fn parse(parser: &mut Parser) -> Result<StructExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let struct_path = PathExpr::parse(parser)?;

        let open_brace = parser.expect_open_brace()?;

        let struct_fields_opt = get_collection(parser, parse_struct_field, &open_brace)?;

        let span = parser.get_braced_item_span(first_token.as_ref())?;

        Ok(StructExpr {
            struct_path,
            struct_fields_opt,
            span,
        })
    }
}

impl fmt::Debug for StructExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StructExpr")
            .field("struct_path", &self.struct_path)
            .field("struct_fields_opt", &self.struct_fields_opt)
            .finish()
    }
}

impl ParseOperatorExpr for TupleStructExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let open_paren = parser.expect_open_paren()?;

        let struct_elements_opt = get_expressions(parser, Precedence::Lowest, &open_paren)?;

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
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.warn_unmatched_delimiter(&open_paren);
                Err(ErrorsEmitted)
            }
            _ => {
                parser.emit_unexpected_token(&TokenType::RParen.to_string());
                Err(ErrorsEmitted)
            }
        }
    }
}

impl fmt::Debug for TupleStructExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TupleStructExpr")
            .field("struct_path", &self.struct_path)
            .field("struct_elements_opt", &self.struct_elements_opt)
            .finish()
    }
}

fn parse_struct_field(parser: &mut Parser) -> Result<StructField, ErrorsEmitted> {
    let attributes_opt = get_attributes(parser, OuterAttr::outer_attr);

    let field_name = parser.expect_identifier("struct field name")?;

    parser.expect_token(TokenType::Colon)?;

    let field_value = match parser.current_token() {
        Some(Token::Comma { .. } | Token::RBrace { .. }) => {
            parser.emit_missing_node("expr", "struct field value");
            parser.next_token();
            Err(ErrorsEmitted)
        }
        Some(Token::EOF) | None => {
            parser.emit_unexpected_eoi();
            parser.emit_missing_node("expr", "struct field value");
            Err(ErrorsEmitted)
        }
        _ => parser.parse_expression(Precedence::Lowest),
    }?;

    let struct_field = StructField {
        attributes_opt,
        field_name,
        field_value: Box::new(field_value),
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
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_tuple_struct_expr() -> Result<(), ()> {
        let input = r#"SomeStruct("bar", false, -1)"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
