use core::fmt;

use crate::{
    ast::{Expression, Type, TypeCastExpr, TypeCastOp},
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    span::Spanned,
    token::{Token, TokenType},
};

impl ParseOperatorExpr for TypeCastExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let value_expr = left_expr.try_into().map_err(|e| {
            parser.emit_error(e);
            ErrorsEmitted
        })?;

        let type_cast_op = if let Some(Token::As { .. }) = parser.current_token() {
            parser.next_token();
            Ok(TypeCastOp)
        } else {
            parser.emit_unexpected_token(&format!("type cast operator ({})", TokenType::As));
            Err(ErrorsEmitted)
        }?;

        let new_type = match parser.current_token() {
            Some(Token::EOF) | None => {
                parser.emit_unexpected_eoi();
                parser.emit_missing_node("type", "cast type");
                Err(ErrorsEmitted)
            }
            Some(
                Token::I32Type { .. }
                | Token::I64Type { .. }
                | Token::U8Type { .. }
                | Token::U16Type { .. }
                | Token::U32Type { .. }
                | Token::U64Type { .. }
                | Token::U256Type { .. }
                | Token::U512Type { .. }
                | Token::F32Type { .. }
                | Token::F64Type { .. }
                | Token::ByteType { .. }
                | Token::B2Type { .. }
                | Token::B4Type { .. }
                | Token::B8Type { .. }
                | Token::B16Type { .. }
                | Token::B32Type { .. }
                | Token::H160Type { .. }
                | Token::H256Type { .. }
                | Token::H512Type { .. }
                | Token::CharType { .. },
            ) => Ok(Type::parse(parser)?),
            _ => {
                parser.emit_unexpected_token("numeric, text or hash cast type");
                Err(ErrorsEmitted)
            }
        }?;

        let last_token = parser.peek_behind_by(1);

        let span = parser.get_span(left_expr_span, &last_token.unwrap().span());

        let expr = TypeCastExpr {
            value: Box::new(value_expr),
            type_cast_op,
            new_type: Box::new(new_type),
            span,
        };

        Ok(Expression::TypeCast(expr))
    }
}

impl fmt::Debug for TypeCastExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeCastExpr")
            .field("value", &self.value)
            .field("new_type", &self.new_type)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        error::ParserErrorKind,
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_type_cast_expr_numeric() -> Result<(), ()> {
        let input = r#"x as u64"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_type_cast_expr_identifier() -> Result<(), ()> {
        let input = r#"x as foo"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        match parser.parse_expression(Precedence::Lowest) {
            Ok(_) => Err(println!("expected `ParserErrorKind::UnexpectedToken` error, but test was successful")),
            Err(_) => match parser.errors.last() {
                Some(err) => match err.error_kind() {
                   ParserErrorKind::UnexpectedToken { .. } => Ok(println!("test failed correctly with error: {err:#?}")),

                   err => Err(println!("test failed with unexpected error. Expected `ParserErrorKind::UnexpectedToken` error, found `{err}`"))
                },
                _ => Err(println!("test failed, but no errors detected. Expected `ParserErrorKind::UnexpectedToken` error, found none"))
            },
        }
    }
}
